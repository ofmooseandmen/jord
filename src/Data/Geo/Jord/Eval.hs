{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Data.Geo.Jord.Eval
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for evaluating expressions in textual form.
--
module Data.Geo.Jord.Eval
    ( Resolve
    , Result
    , Value(..)
    , eval
    , eval'
    , functions
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Fail
import Data.Geo.Jord.Angle
import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.GreatCircle
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.List
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP

-- | A value accepted and returned by 'eval'.
data Value
    = Ang Angle -- ^ 'Angle'
    | Len Length -- ^ 'Length'
    | Geo GeoPos -- ^ 'GeoPos'
    | Vec NVector -- ^ 'NVector'
    | Ll (Double, Double) -- ^ tuple of latitude and longitude
    deriving (Eq, Show)

-- | 'Either' an error or a 'Value'.
type Result = Either String Value

-- | Resolves a value by its name if it exists.
type Resolve = String -> Maybe Value

instance MonadFail (Either String) where
    fail = Left

-- | Evaluates @s@, an expression of the form @"(f x y ..)"@.
--
-- >>> eval "finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readGeoPos 54°N,154°E)"
-- 126°
--
-- @f@ must be one of the supported 'functions' and each parameter @x@, @y@, .. , is either another function call
-- or a 'String' parameter. Parameters are either resolved by name using the 'Resolve'
-- function @r@ or if it returns 'Nothing', 'read' to an 'Angle', a 'Length' or a 'GeoPos'.
--
-- If the evaluation is successful, returns the resulting 'Value' ('Right') otherwise
-- a description of the error ('Left').
--
-- @
--     angle = eval "finalBearing 54N154E 54S154W" -- Right Ang
--     length = eval "distance (antipode 54N154E) 54S154W" -- Right Len
--     -- parameter resolution by name
--     m = Map.empty -- assuming use of Data.Map
--     a1 = eval "finalBearing 54N154E 54S154W"
--     m = insert "a1" a
--     a2 = eval' (\\k -> lookup k m) "(finalBearing a1 54S154W)"
-- @
--
-- By default, all returned positions are 'Geo' ('GeoPos'), to get back a 'Vec' ('NVector'), the
-- expression must be wrapped by 'toNVector'.
--
-- @
--     dest = eval "destination 54°N,154°E 54° 1000m" -- Right Geo
--     dest = eval "toNVector (destination 54°N,154°E 54° 1000m)" -- Right Vec
-- @
--
-- Every function call must be wrapped between parentheses, however they can be ommitted for the top level call.
--
-- @
--     angle = eval "finalBearing 54N154E 54S154W" -- Right Ang
--     angle = eval "(finalBearing 54N154E 54S154W)" -- Right Ang
--     length = eval "distance (antipode 54N154E) 54S154W" -- Right Len
--     length = eval "distance antipode 54N154E 54S154W" -- Left String
-- @
--
eval :: String -> Resolve -> Result
eval s r =
    case expr s of
        Left err -> Left err
        Right (rvec, ex) ->
            case evalExpr ex r of
                vec@(Right (Vec v)) ->
                    if rvec
                        then vec
                        else Right (Geo (fromNVector v))
                oth -> oth

-- | Same as 'eval' but never resolves parameters by name.
eval' :: String -> Result
eval' s = eval s (const Nothing)

-- | All supported functions:
--
--     * 'antipode'
--
--     * 'destination'
--
--     * 'distance'
--
--     * 'finalBearing'
--
--     * 'initialBearing'
--
--     * 'latlong'
--
--     * 'midpoint'
--
--     * 'readGeoPos'
--
--     * 'toNVector'
--
functions :: [String]
functions =
    [ "antipode"
    , "destination"
    , "distance"
    , "finalBearing"
    , "initialBearing"
    , "latlong"
    , "midpoint"
    , "readGeoPos"
    , "toNVector"
    ]

expr :: (MonadFail m) => String -> m (Bool, Expr)
expr s = do
    ts <- tokenise s
    ast <- parse ts
    fmap (\a -> (expectVec ts, a)) (transform ast)

expectVec :: [Token] -> Bool
expectVec (_:Func "toNVector":_) = True
expectVec _ = False

evalExpr :: Expr -> Resolve -> Result
evalExpr (Param p) f =
    case f p of
        Just (Geo g) -> Right (Vec (toNVector g))
        Just v -> Right v
        Nothing -> tryRead p
evalExpr (Antipode a) f =
    case evalExpr a f of
        (Right (Vec p)) -> Right (Vec (antipode p))
        r -> Left ("Call error: antipode " ++ showErr [r])
evalExpr (Destination a b c) f =
    case [evalExpr a f, evalExpr b f, evalExpr c f] of
        [Right (Vec p), Right (Ang a'), Right (Len l)] -> Right (Vec (destination p a' l))
        r -> Left ("Call error: destination " ++ showErr r)
evalExpr (Distance a b) f =
    case [evalExpr a f, evalExpr b f] of
        [Right (Vec p1), Right (Vec p2)] -> Right (Len (distance p1 p2))
        r -> Left ("Call error: distance " ++ showErr r)
evalExpr (FinalBearing a b) f =
    case [evalExpr a f, evalExpr b f] of
        [Right (Vec p1), Right (Vec p2)] -> Right (Ang (finalBearing p1 p2))
        r -> Left ("Call error: finalBearing " ++ showErr r)
evalExpr (InitialBearing a b) f =
    case [evalExpr a f, evalExpr b f] of
        [Right (Vec p1), Right (Vec p2)] -> Right (Ang (initialBearing p1 p2))
        r -> Left ("Call error: initialBearing " ++ showErr r)
evalExpr (LatLong a) f =
    case evalExpr a f of
        (Right (Vec p)) ->
            let g = fromNVector p
             in Right (Ll (degrees (latitude g), degrees (longitude g)))
        r -> Left ("Call error: latlong " ++ showErr [r])
evalExpr (Midpoint as) f =
    let m = map (`evalExpr` f) as
        ps = [p | Right (Vec p) <- m]
     in if length m == length ps
            then Right (Vec (midpoint ps))
            else Left ("Call error: midpoint " ++ showErr m)
evalExpr (ReadGeoPos s) _ =
    maybe (Left ("Call error: readGeoPos : " ++ s)) (Right . Vec . toNVector) (readGeoPosM s)
evalExpr (ToNVector a) f =
    case evalExpr a f of
        r@(Right (Vec _)) -> r
        r -> Left ("Call error: toNVector " ++ showErr [r])

showErr :: [Result] -> String
showErr rs = " > " ++ intercalate " & " (map (either id show) rs)

tryRead :: String -> Result
tryRead s =
    case r of
        [a@(Right (Ang _)), _, _] -> a
        [_, l@(Right (Len _)), _] -> l
        [_, _, Right (Geo g)] -> Right (Vec (toNVector g))
        _ -> Left ("couldn't resolve " ++ s)
  where
    r = map ($ s) [readM readAngleM Ang, readM readLengthM Len, readM readGeoPosM Geo]

readM :: (String -> Maybe a) -> (a -> Value) -> String -> Either String Value
readM p r s = maybe (Left ("Invalid text [" ++ s ++ "]")) (Right . r) (p s)

------------------------------------------
--  Lexical Analysis: String -> [Token] --
------------------------------------------
data Token
    = Paren Char
    | Func String
    | Str String
    deriving (Show)

tokenise :: (MonadFail m) => String -> m [Token]
tokenise s =
    case last (readP_to_S tokens (wrap s)) of
        (e, "") -> return e
        r -> fail ("Lexical error: " ++ snd r)

wrap :: String -> String
wrap s
    | head s /= '(' = '(' : s ++ [')']
    | otherwise = s

tokens :: ReadP [Token]
tokens = many1 token

token :: ReadP Token
token = (<++) ((<++) paren func) str

paren :: ReadP Token
paren = parenO <|> parenC

parenO :: ReadP Token
parenO = do
    c <- char '('
    return (Paren c)

parenC :: ReadP Token
parenC = do
    c <- char ')'
    optional (char ' ')
    return (Paren c)

func :: ReadP Token
func = do
    n <- choice (map string functions)
    _ <- char ' '
    return (Func n)

str :: ReadP Token
str = do
    optional (char ' ')
    v <- munch1 (\c -> c /= '(' && c /= ')' && c /= ' ')
    if v `elem` functions
        then pfail
        else return (Str v)

-----------------------------------------
--  Syntactic Analysis: [Token] -> Ast --
-----------------------------------------
data Ast
    = Call String
           [Ast]
    | Lit String
    deriving (Show)

-- | syntax is (f x y) where x and y can be function themselves.
parse :: (MonadFail m) => [Token] -> m Ast
parse ts = fmap fst (walk ts)

walk :: (MonadFail m) => [Token] -> m (Ast, [Token])
walk [] = fail "Syntax error: empty"
walk (h:t)
    | (Str s) <- h = return (Lit s, t)
    | (Paren '(') <- h = walkFunc t
    | otherwise = fail ("Syntax error: expected String or '(' but got " ++ show h)

walkFunc :: (MonadFail m) => [Token] -> m (Ast, [Token])
walkFunc [] = fail "Syntax error: '(' unexpected"
walkFunc (h:t)
    | (Func n) <- h = walkParams n t []
    | otherwise = fail ("Syntax error: expected Function but got " ++ show h)

walkParams :: (MonadFail m) => String -> [Token] -> [Ast] -> m (Ast, [Token])
walkParams _ [] _ = fail "Syntax error: ')' not found"
walkParams n ts@(h:t) acc
    | (Paren ')') <- h = return (Call n (reverse acc), t)
    | otherwise = do
        (el, t') <- walk ts
        walkParams n t' (el : acc)

-------------------------------------
--  Semantic Analysis: Ast -> Expr --
-------------------------------------
data Expr
    = Param String
    | Antipode Expr
    | Destination Expr
                  Expr
                  Expr
    | Distance Expr
               Expr
    | FinalBearing Expr
                   Expr
    | InitialBearing Expr
                     Expr
    | LatLong Expr
    | Midpoint [Expr]
    | ReadGeoPos String
    | ToNVector Expr
    deriving (Show)

transform :: (MonadFail m) => Ast -> m Expr
transform (Call "antipode" [e]) = fmap Antipode (transform e)
transform (Call "destination" [e1, e2, e3]) = do
    p1 <- transform e1
    p2 <- transform e2
    p3 <- transform e3
    return (Destination p1 p2 p3)
transform (Call "distance" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (Distance p1 p2)
transform (Call "finalBearing" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (FinalBearing p1 p2)
transform (Call "initialBearing" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (InitialBearing p1 p2)
transform (Call "latlong" [e]) = fmap LatLong (transform e)
transform (Call "midpoint" e) = do
    ps <- mapM transform e
    return (Midpoint ps)
transform (Call "readGeoPos" [Lit s]) = return (ReadGeoPos s)
transform (Call "toNVector" [e]) = fmap ToNVector (transform e)
transform (Call f e) = fail ("Semantic error: " ++ f ++ " does not accept " ++ show e)
transform (Lit s) = return (Param s)
