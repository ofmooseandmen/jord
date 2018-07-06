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
    ( Result
    , Value(..)
    , eval
    , eval'
    , functions
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Fail
import Data.Char (isAlphaNum)
import Data.Geo.Jord.Angle
import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.GreatCircle
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP

-- | A value representing all data types used by "Jord".
data Value
    = Ang Angle -- ^ 'Angle'
    | Len Length -- ^ 'Length'
    | Geo GeoPos -- ^ 'GeoPos'
    | Vec NVector -- ^ 'NVector'
    deriving (Eq, Show)

-- | 'Either' an error or a 'Value'.
type Result = Either String Value

instance MonadFail (Either String) where
    fail = Left

-- | Evaluates @s@, an expression of the form @"(f x y ..)"@ where @f@ is one of the
-- supported 'functions' and each parameter @x@, @y@, .. , is either another function call
-- or a 'String' that can be 'read' into an 'Angle', a 'Length' or a 'GeoPos'.
-- Returns 'Either' the resulting 'Value' ('Right') or the description of the error ('Left')
--
-- @
--     angle = eval "finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readGeoPos 54°N,154°E)"
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
eval :: String -> Result
eval s = do
    case expr s of
        Left err -> Left err
        Right ex ->
            case evalExpr ex of
                (Right (Vec v)) -> (Right (Geo (fromNVector v)))
                r -> r

-- | Same as 'eval' but accepts a function @r@ that is used to resolve
-- parameters by name. If the function returns 'Nothing', the parameter is 'read'
-- as described in 'eval'.
--
-- @
--     m = Map.empty -- assuming use of Data.Map
--     a1 = eval "finalBearing 54N154E 54S154W"
--     m = insert "a1" a
--     a2 = eval' (\k -> lookup k m) "(finalBearing a1 54S154W)"
-- @
--
eval' :: String -> (String -> Maybe Value) -> Result
eval' = Left "TODO"

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
--     * 'readGeoPos'
--
functions :: [String]
functions = ["antipode", "destination", "distance", "finalBearing", "initialBearing", "readGeoPos"]

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
    | ReadGeoPos String
    deriving (Show)

expr :: (MonadFail m) => String -> m Expr
expr s = do
    ts <- tokenise s
    ast <- parse ts
    transform ast

transform :: (MonadFail m) => Ast -> m Expr
transform (Call "antipode" [e]) = fmap Antipode (transform e)
transform (Call "destination" (e1:e2:e3:[])) = do
    p1 <- transform e1
    p2 <- transform e2
    p3 <- transform e3
    return (Destination p1 p2 p3)
transform (Call "distance" (e1:e2:[])) = do
    p1 <- transform e1
    p2 <- transform e2
    return (Distance p1 p2)
transform (Call "finalBearing" (e1:e2:[])) = do
    p1 <- transform e1
    p2 <- transform e2
    return (FinalBearing p1 p2)
transform (Call "initialBearing" (e1:e2:[])) = do
    p1 <- transform e1
    p2 <- transform e2
    return (InitialBearing p1 p2)
transform (Call "readGeoPos" [(Lit s)]) = return (ReadGeoPos s)
transform (Call f e) = fail ("Semantic error: " ++ f ++ " does not accept " ++ show e)
transform (Lit s) = return (Param s)

evalExpr :: Expr -> Result
evalExpr (Param p) =
    case r of
        [(Right (Ang a)), _, _] -> Right (Ang a)
        [_, (Right (Len l)), _] -> Right (Len l)
        [_, _, (Right (Geo g))] -> Right (Vec (toNVector g))
        _ -> Left ("Parameter error: couldn't resolve " ++ p)
  where
    r = map ($ p) [(readM readAngleM Ang), (readM readLengthM Len), (readM readGeoPosM Geo)]
evalExpr (Antipode a) =
    case evalExpr a of
        (Right (Vec p)) -> Right (Vec (antipode p))
        r -> Left ("Call error: antipode " ++ show r)
evalExpr (Destination a b c) =
    case [evalExpr a, evalExpr b, evalExpr c] of
        [(Right (Vec p)), (Right (Ang a')), (Right (Len l))] -> Right (Vec (destination p a' l))
        r -> Left ("Call error: destination " ++ show r)
evalExpr (Distance a b) =
    case [evalExpr a, evalExpr b] of
        [(Right (Vec p1)), (Right (Vec p2))] -> Right (Len (distance p1 p2))
        r -> Left ("Call error: distance " ++ show r)
evalExpr (FinalBearing a b) =
    case [evalExpr a, evalExpr b] of
        [(Right (Vec p1)), (Right (Vec p2))] -> Right (Ang (finalBearing p1 p2))
        r -> Left ("Call error: finalBearing " ++ show r)
evalExpr (InitialBearing a b) =
    case [evalExpr a, evalExpr b] of
        [(Right (Vec p1)), (Right (Vec p2))] -> Right (Ang (initialBearing p1 p2))
        r -> Left ("Call error: initialBearing " ++ show r)
evalExpr (ReadGeoPos s) =
    maybe
        (Left ("Call error: readGeoPos : " ++ s))
        (\g -> Right (Vec (toNVector g)))
        (readGeoPosM s)

readM :: (String -> Maybe a) -> (a -> Value) -> String -> Either String Value
readM p r s = maybe (Left ("Invalid text: " ++ s)) (\v -> Right (r v)) (p s)

-- Lexical Analysis: String -> [Token]
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
paren = do
    skipSpaces
    c <- char '(' <|> char ')'
    skipSpaces
    return (Paren c)

func :: ReadP Token
func = do
    n <- choice (map string functions)
    _ <- char ' '
    return (Func n)

str :: ReadP Token
str = do
    v <- munch1 (\c -> isAlphaNum c || c == '°' || c == '\'' || c == '"' || c == ',')
    skipSpaces
    return (Str v)

-- | Syntactic Analysis: [Token] -> Ast
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
    | otherwise = fail ("Syntax error: expected String or '(' but got " ++ (show h))

walkFunc :: (MonadFail m) => [Token] -> m (Ast, [Token])
walkFunc [] = fail "Syntax error: '(' unexpected"
walkFunc (h:t)
    | (Func n) <- h = walkParams n t []
    | otherwise = fail ("Syntax error: expected Function but got " ++ (show h))

walkParams :: (MonadFail m) => String -> [Token] -> [Ast] -> m (Ast, [Token])
walkParams _ [] _ = fail "Syntax error: ')' not found"
walkParams n ts@(h:t) acc
    | (Paren ')') <- h = return (Call n (reverse acc), t)
    | otherwise = do
        (el, t') <- walk ts
        walkParams n t' (el : acc)
