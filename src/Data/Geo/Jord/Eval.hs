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
    ( Value(..)
    , Vault
    , Result
    , emptyVault
    , eval
    , functions
    , insert
    , delete
    , lookup
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Fail
import Data.Bifunctor
import Data.Geo.Jord.Angle
import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.GreatCircle
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.List hiding (delete, lookup, insert)
import Prelude hiding (fail, lookup)
import Text.ParserCombinators.ReadP

-- | A value accepted and returned by 'eval'.
data Value
    = Ang Angle -- ^ 'Angle'
    | Len Length -- ^ 'Length'
    | Geo GeoPos -- ^ 'GeoPos'
    | Vec NVector -- ^ 'NVector'
    | GeoDec (Double, Double) -- ^ tuple of decimal latitude and longitude
    deriving (Eq, Show)

-- | 'Either' an error or a 'Value'.
type Result = Either String Value

-- | A location for 'Value's to be shared by successive evalations.
newtype Vault =
    Vault [(String, Value)]

-- | An empty 'Vault'.
emptyVault :: Vault
emptyVault = Vault []

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
--     vault = emptyVault
--     angle = eval "finalBearing 54N154E 54S154W" vault -- Right Ang
--     length = eval "distance (antipode 54N154E) 54S154W" vault -- Right Len
--     -- parameter resolution from vault
--     a1 = eval "finalBearing 54N154E 54S154W" vault
--     vault = insert "a1" vault
--     a2 = eval "(finalBearing a1 54S154W)" vault
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
eval :: String -> Vault -> Result
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

-- | All supported functions:
--
--     * 'antipode'
--
--     * 'decimalLatLong'
--
--     * 'destination'
--
--     * 'distance'
--
--     * 'finalBearing'
--
--     * 'initialBearing'
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
    , "decimalLatLong"
    , "destination"
    , "distance"
    , "finalBearing"
    , "initialBearing"
    , "midpoint"
    , "readGeoPos"
    , "toNVector"
    ]

-- | @insert k v vault@ inserts value @v@ for key @k@. Overwrites any previous value.
insert :: String -> Value -> Vault -> Vault
insert k v vault = Vault (e ++ [(k, v)])
  where
    Vault e = delete k vault

-- | @lookup k vault@ looks up the value of key @k@ in the vault.
lookup :: String -> Vault -> Maybe Value
lookup k (Vault es) = fmap snd (find (\e -> fst e == k) es)

-- | @delete k vault@ deletes key @k@ from the vault.
delete :: String -> Vault -> Vault
delete k (Vault es) = Vault (filter (\e -> fst e /= k) es)

expr :: (MonadFail m) => String -> m (Bool, Expr)
expr s = do
    ts <- tokenise s
    ast <- parse ts
    fmap (\a -> (expectVec ts, a)) (transform ast)

expectVec :: [Token] -> Bool
expectVec (_:Func "toNVector":_) = True
expectVec _ = False

evalExpr :: Expr -> Vault -> Result
evalExpr (Param p) vault =
    case lookup p vault of
        Just (Geo g) -> Right (Vec (toNVector g))
        Just v -> Right v
        Nothing -> tryRead p
evalExpr (Antipode a) vault =
    case evalExpr a vault of
        (Right (Vec p)) -> Right (Vec (antipode p))
        r -> Left ("Call error: antipode " ++ showErr [r])
evalExpr (DecimalLatLong a) vault =
    case evalExpr a vault of
        (Right (Vec p)) ->
            let g = fromNVector p
             in Right (GeoDec (toDecimalDegrees (latitude g), toDecimalDegrees (longitude g)))
        r -> Left ("Call error: decimalLatLong " ++ showErr [r])
evalExpr (Destination a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault] of
        [Right (Vec p), Right (Ang a'), Right (Len l)] -> Right (Vec (destination' p a' l))
        r -> Left ("Call error: destination " ++ showErr r)
evalExpr (Distance a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Vec p1), Right (Vec p2)] -> Right (Len (distance' p1 p2))
        r -> Left ("Call error: distance " ++ showErr r)
evalExpr (FinalBearing a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Vec p1), Right (Vec p2)] -> Right (Ang (finalBearing p1 p2))
        r -> Left ("Call error: finalBearing " ++ showErr r)
evalExpr (InitialBearing a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Vec p1), Right (Vec p2)] -> Right (Ang (initialBearing p1 p2))
        r -> Left ("Call error: initialBearing " ++ showErr r)
evalExpr (Midpoint as) vault =
    let m = map (`evalExpr` vault) as
        ps = [p | Right (Vec p) <- m]
     in if length m == length ps
            then Right (Vec (midpoint ps))
            else Left ("Call error: midpoint " ++ showErr m)
evalExpr (ReadGeoPos s) _ =
    bimap (\e -> "Call error: readGeoPos : " ++ e) (Vec . toNVector) (readGeoPosE s)
evalExpr (ToNVector a) vault =
    case evalExpr a vault of
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
        _ -> Left ("couldn't read " ++ s)
  where
    r = map ($ s) [readE readAngleE Ang, readE readLengthE Len, readE readGeoPosE Geo]

readE :: (String -> Either String a) -> (a -> Value) -> String -> Either String Value
readE p v s = bimap id v (p s)

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
    | DecimalLatLong Expr
    | Destination Expr
                  Expr
                  Expr
    | Distance Expr
               Expr
    | FinalBearing Expr
                   Expr
    | InitialBearing Expr
                     Expr
    | Midpoint [Expr]
    | ReadGeoPos String
    | ToNVector Expr
    deriving (Show)

transform :: (MonadFail m) => Ast -> m Expr
transform (Call "antipode" [e]) = fmap Antipode (transform e)
transform (Call "decimalLatLong" [e]) = fmap DecimalLatLong (transform e)
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
transform (Call "midpoint" e) = do
    ps <- mapM transform e
    return (Midpoint ps)
transform (Call "readGeoPos" [Lit s]) = return (ReadGeoPos s)
transform (Call "toNVector" [e]) = fmap ToNVector (transform e)
transform (Call f e) = fail ("Semantic error: " ++ f ++ " does not accept " ++ show e)
transform (Lit s) = return (Param s)
