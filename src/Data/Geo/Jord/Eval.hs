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
    ( Result(..)
    , eval
    , functions
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Fail
import Data.Char (isAlphaNum)
import Data.Geo.Jord.Angle
import Data.Geo.Jord.GeoPos
import Data.Geo.Jord.GreatCircle
import Data.Geo.Jord.Length
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP

-- | Result of an evaluation
data Result
    = Error String -- ^ Error with message
    | Ang Angle -- ^ 'Angle'
    | Len Length -- ^ 'Length'
    | Geo GeoPos -- ^ 'GeoPos'
    deriving (Eq, Show)

instance MonadFail (Either String) where
    fail = Left

-- | Evaluates an expression @"(f x y ..)"@ where @f@ is one of the
-- supported 'functions' and @x@, @y@, .. , are either another function call
-- or a 'String' that can be read into an 'Angle', a 'Length' or a 'GeoPos'.
--
-- @
--     angle = eval "finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readGeoPos 54°N,154°E)"
-- @
--
-- Every function call must be wrapped between parentheses, however they can be ommitted for the top level call.
--
-- @
--     angle = eval "finalBearing 54N154E 54S154W" -- OK
--     angle = eval "(finalBearing 54N154E 54S154W)" -- OK
--     length = eval "distance (antipode 54N154E) 54S154W" -- OK
--     length = eval "distance antipode 54N154E 54S154W" -- NOK
-- @
--
eval :: String -> Result
eval s = do
    case expr s of
        Left m -> Error m
        Right e -> evalExpr e

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
        [(Ang a), _, _] -> Ang a
        [_, (Len l), _] -> Len l
        [_, _, (Geo g)] -> Geo g
        _ -> Error ("Parameter error: couldn't resolve " ++ p)
  where
    r = map ($ p) [(readM readAngleM Ang), (readM readLengthM Len), (readM readGeoPosM Geo)]
evalExpr (Antipode a) =
    case evalExpr a of
        (Geo p) -> Geo (antipode p)
        r -> Error ("Call error: antipode " ++ show r)
evalExpr (Destination a b c) =
    case [evalExpr a, evalExpr b, evalExpr c] of
        [(Geo p), (Ang a'), (Len l)] -> Geo (destination p a' l)
        r -> Error ("Call error: destination " ++ show r)
evalExpr (Distance a b) =
    case [evalExpr a, evalExpr b] of
        [(Geo p1), (Geo p2)] -> Len (distance p1 p2)
        r -> Error ("Call error: distance " ++ show r)
evalExpr (FinalBearing a b) =
    case [evalExpr a, evalExpr b] of
        [(Geo p1), (Geo p2)] -> Ang (finalBearing p1 p2)
        r -> Error ("Call error: finalBearing " ++ show r)
evalExpr (InitialBearing a b) =
    case [evalExpr a, evalExpr b] of
        [(Geo p1), (Geo p2)] -> Ang (initialBearing p1 p2)
        r -> Error ("Call error: initialBearing " ++ show r)
evalExpr (ReadGeoPos s) = maybe (Error ("Call error: readGeoPos : " ++ s)) Geo (readGeoPosM s)

readM :: (String -> Maybe a) -> (a -> Result) -> String -> Result
readM p r s = maybe (Error ("Invalid text: " ++ s)) r (p s)

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
