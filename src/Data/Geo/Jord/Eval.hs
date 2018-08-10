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

import Control.Monad.Fail
import Data.Bifunctor
import Data.Geo.Jord.Angle
import Data.Geo.Jord.AngularPosition
import Data.Geo.Jord.Geodetics
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Transform
import Data.List hiding (delete, insert, lookup)
import Data.Maybe
import Prelude hiding (fail, lookup)
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

-- | A value accepted and returned by 'eval'.
data Value
    = Ang Angle -- ^ 'Angle'
    | AngDec Double -- ^ 'Angle' in decimal degrees
    | Bool Bool -- ^ boolean
    | Double Double -- ^ double
    | Len Length -- ^ 'Length'
    | Gc GreatCircle -- ^ 'GreatCircle'
    | Llh (AngularPosition LatLong) -- ^ 'AngularPosition' of 'LatLong'
    | Llhs [AngularPosition LatLong] -- ^ list of 'AngularPosition' of 'LatLong'
    | LlDec (Double, Double) -- ^ latitude and longitude in decimal degrees
    | LlsDec [(Double, Double)] -- ^ list of latitude and longitude in decimal degrees
    | Nvh (AngularPosition NVector) -- ^ 'AngularPosition' of n-vector
    | Nvhs [AngularPosition NVector] -- ^ list of 'AngularPosition' of n-vectors
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
-- >>> eval "finalBearing (destination (antipode 54°N,154°E) 54° 1000m) 54°N,154°E"
-- 126°
--
-- @f@ must be one of the supported 'functions' and each parameter @x@, @y@, .. , is either another function call
-- or a 'String' parameter. Parameters are either resolved by name using the 'Resolve'
-- function @r@ or if it returns 'Nothing', 'read' to an 'Angle', a 'Length' or a 'LatLong'.
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
-- All returned positions are 'LatLong' by default, to get back a n-vector the
-- expression must be wrapped by 'toNVector'.
--
-- @
--     dest = eval "destination 54°N,154°E 54° 1000m" -- Right Ll
--     dest = eval "toNVector (destination 54°N,154°E 54° 1000m)" -- Right Nvh
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
        Right (rvec, expr') -> convert (evalExpr expr' r) rvec

convert :: Result -> Bool -> Result
convert r True = r
convert r False =
    case r of
        Right (Nvh v) -> Right (Llh (fromNVector v))
        Right (Nvhs vs) -> Right (Llhs (map fromNVector vs))
        oth -> oth

-- | All supported functions: antipode, crossTrackDistance, decimalDegrees,
-- decimalLatLong, destination, finalBearing, greatCircle, initialBearing, interpolate,
-- intersections, insideSurface, mean, latLong, readLatLong, surfaceDistance,
-- toDecimalDegrees, toKilometres, toMetres, toNauticalMiles, toNVector.
functions :: [String]
functions =
    [ "antipode"
    , "crossTrackDistance"
    , "destination"
    , "decimalDegrees"
    , "decimalLatLong"
    , "finalBearing"
    , "greatCircle"
    , "initialBearing"
    , "interpolate"
    , "intersections"
    , "insideSurface"
    , "latLong"
    , "mean"
    , "readLatLong"
    , "surfaceDistance"
    , "toDecimalDegrees"
    , "toKilometres"
    , "toMetres"
    , "toNauticalMiles"
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
        Just (Llh llh) -> Right (Nvh (toNVector llh))
        Just v -> Right v
        Nothing -> tryRead p
evalExpr (Antipode a) vault =
    case evalExpr a vault of
        (Right (Nvh p)) -> Right (Nvh (antipode p))
        r -> Left ("Call error: antipode " ++ showErr [r])
evalExpr (CrossTrackDistance a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Nvh p), Right (Gc gc)] -> Right (Len (crossTrackDistance84 p gc))
        r -> Left ("Call error: crossTrackDistance " ++ showErr r)
evalExpr (DecimalDegrees d) _ = Right (Ang (decimalDegrees d))
evalExpr (DecimalLatLong a b) _ =
    bimap (\e -> "Call error: decimalLatLong : " ++ e) (Nvh . toNVector) (decimalLatLongE a b)
evalExpr (Destination a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault] of
        [Right (Nvh p), Right (Ang a'), Right (Len l)] -> Right (Nvh (destination84 p a' l))
        r -> Left ("Call error: destination " ++ showErr r)
evalExpr (FinalBearing a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Nvh p1), Right (Nvh p2)] -> Right (Ang (finalBearing p1 p2))
        r -> Left ("Call error: finalBearing " ++ showErr r)
evalExpr (GreatCircleSC a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Nvh p1), Right (Nvh p2)] -> bimap id Gc (greatCircleE p1 p2)
        [Right (Nvh p), Right (Ang a')] -> Right (Gc (greatCircleBearing p a'))
        r -> Left ("Call error: greatCircle " ++ showErr r)
evalExpr (InitialBearing a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Nvh p1), Right (Nvh p2)] -> Right (Ang (initialBearing p1 p2))
        r -> Left ("Call error: initialBearing " ++ showErr r)
evalExpr (Interpolate a b c) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Nvh p1), Right (Nvh p2)] -> Right (Nvh (interpolate p1 p2 c))
        r -> Left ("Call error: interpolate " ++ showErr r)
evalExpr (Intersections a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Gc gc1), Right (Gc gc2)] ->
            maybe
                (Right (Nvhs []))
                (\is -> Right (Nvhs [fst is, snd is]))
                (intersections gc1 gc2 :: Maybe (AngularPosition NVector, AngularPosition NVector))
        r -> Left ("Call error: intersections " ++ showErr r)
evalExpr (InsideSurface as) vault =
    let m = map (`evalExpr` vault) as
        ps = [p | Right (Nvh p) <- m]
     in if length m == length ps && length ps > 3
            then Right (Bool (insideSurface (head ps) (tail ps)))
            else Left ("Call error: insideSurface " ++ showErr m)
evalExpr (Mean as) vault =
    let m = map (`evalExpr` vault) as
        ps = [p | Right (Nvh p) <- m]
     in if length m == length ps
            then maybe (Left ("Call error: mean " ++ showErr m)) (Right . Nvh) (mean ps)
            else Left ("Call error: mean " ++ showErr m)
evalExpr (LatLong a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Ang lat), Right (Ang lon)] ->
            bimap (\e -> "Call error: latLong : " ++ e) (Nvh . toNVector) (latLongE lat lon)
        r -> Left ("Call error: latLong " ++ showErr r)
evalExpr (ReadLatLong s) _ =
    bimap (\e -> "Call error: readLatLong : " ++ e) (Nvh . toNVector) (readLatLongE s)
evalExpr (SurfaceDistance a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Nvh p1), Right (Nvh p2)] -> Right (Len (surfaceDistance84 p1 p2))
        r -> Left ("Call error: surfaceDistance " ++ showErr r)
evalExpr (ToDecimalDegrees e) vault =
    case evalExpr e vault of
        (Right (Ang a)) -> Right (AngDec (toDecimalDegrees a))
        (Right (Llh p)) -> Right (LlDec (toDecimalDegrees' (pos p)))
        (Right (Llhs ps)) -> Right (LlsDec (map (toDecimalDegrees' . pos) ps))
        (Right (Nvh p)) -> Right (LlDec ((toDecimalDegrees' . pos . fromNVector) p))
        (Right (Nvhs ps)) -> Right (LlsDec (map (toDecimalDegrees' . pos . fromNVector) ps))
        r -> Left ("Call error: toDecimalDegrees" ++ showErr [r])
evalExpr (ToKilometres e) vault =
    case evalExpr e vault of
        (Right (Len l)) -> Right (Double (toKilometres l))
        r -> Left ("Call error: toKilometres" ++ showErr [r])
evalExpr (ToMetres e) vault =
    case evalExpr e vault of
        (Right (Len l)) -> Right (Double (toMetres l))
        r -> Left ("Call error: toMetres" ++ showErr [r])
evalExpr (ToNauticalMiles e) vault =
    case evalExpr e vault of
        (Right (Len l)) -> Right (Double (toNauticalMiles l))
        r -> Left ("Call error: toNauticalMiles" ++ showErr [r])
evalExpr (ToNVector a) vault =
    case evalExpr a vault of
        r@(Right (Nvh _)) -> r
        r -> Left ("Call error: toNVector " ++ showErr [r])

showErr :: [Result] -> String
showErr rs = " > " ++ intercalate " & " (map (either id show) rs)

tryRead :: String -> Result
tryRead s =
    case r of
        [a@(Right (Ang _)), _, _] -> a
        [_, l@(Right (Len _)), _] -> l
        [_, _, Right (Llh llh)] -> Right (Nvh (toNVector llh))
        _ -> Left ("couldn't read " ++ s)
  where
    r = map ($ s) [readE readAngleE Ang, readE readLengthE Len, readE readLatLongE (\ll -> Llh (AngularPosition ll zero))]

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
tokenise s
    | null r = fail ("Lexical error: " ++ s)
    | (e, "") <- last r = return (wrap e)
    | otherwise = fail ("Lexical error: " ++ snd (last r))
  where
    r = readP_to_S tokens s

-- | wraps top level expression between () if needed.
wrap :: [Token] -> [Token]
wrap ts
    | null ts = ts
    | (Paren '(') <- head ts = ts
    | otherwise = Paren '(' : ts ++ [Paren ')']

tokens :: ReadP [Token]
tokens = many1 token

token :: ReadP Token
token = (<++) ((<++) paren func) str

paren :: ReadP Token
paren = (<++) parenO parenC

parenO :: ReadP Token
parenO = do
    optional (char ' ')
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
    | CrossTrackDistance Expr
                         Expr
    | DecimalDegrees Double
    | DecimalLatLong Double
                     Double
    | Destination Expr
                  Expr
                  Expr
    | FinalBearing Expr
                   Expr
    | GreatCircleSC Expr
                    Expr
    | InitialBearing Expr
                     Expr
    | Interpolate Expr
                  Expr
                  Double
    | Intersections Expr
                    Expr
    | InsideSurface [Expr]
    | Mean [Expr]
    | LatLong Expr
              Expr
    | ReadLatLong String
    | SurfaceDistance Expr
                      Expr
    | ToDecimalDegrees Expr
    | ToKilometres Expr
    | ToMetres Expr
    | ToNauticalMiles Expr
    | ToNVector Expr
    deriving (Show)

transform :: (MonadFail m) => Ast -> m Expr
transform (Call "antipode" [e]) = fmap Antipode (transform e)
transform (Call "crossTrackDistance" [e1, e2]) = do
    p <- transform e1
    gc <- transform e2
    return (CrossTrackDistance p gc)
transform (Call "decimalDegrees" [Lit s]) = fmap DecimalDegrees (readDouble s)
transform (Call "decimalLatLong" [Lit s1, Lit s2]) = do
    d1 <- readDouble s1
    d2 <- readDouble s2
    return (DecimalLatLong d1 d2)
transform (Call "destination" [e1, e2, e3]) = do
    p1 <- transform e1
    p2 <- transform e2
    p3 <- transform e3
    return (Destination p1 p2 p3)
transform (Call "finalBearing" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (FinalBearing p1 p2)
transform (Call "greatCircle" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (GreatCircleSC p1 p2)
transform (Call "initialBearing" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (InitialBearing p1 p2)
transform (Call "interpolate" [e1, e2, Lit s]) = do
    p1 <- transform e1
    p2 <- transform e2
    d <- readDouble s
    if d >= 0.0 && d <= 1.0
        then return (Interpolate p1 p2 d)
        else fail "Semantic error: interpolate expects [0..1] as last argument"
transform (Call "intersections" [e1, e2]) = do
    gc1 <- transform e1
    gc2 <- transform e2
    return (Intersections gc1 gc2)
transform (Call "insideSurface" e) = do
    ps <- mapM transform e
    return (InsideSurface ps)
transform (Call "latLong" [e1, e2]) = do
    gc1 <- transform e1
    gc2 <- transform e2
    return (LatLong gc1 gc2)
transform (Call "mean" e) = do
    ps <- mapM transform e
    return (Mean ps)
transform (Call "readLatLong" [Lit s]) = return (ReadLatLong s)
transform (Call "surfaceDistance" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (SurfaceDistance p1 p2)
transform (Call "toDecimalDegrees" [e]) = fmap ToDecimalDegrees (transform e)
transform (Call "toKilometres" [e]) = fmap ToKilometres (transform e)
transform (Call "toMetres" [e]) = fmap ToMetres (transform e)
transform (Call "toNauticalMiles" [e]) = fmap ToNauticalMiles (transform e)
transform (Call "toNVector" [e]) = fmap ToNVector (transform e)
transform (Call f e) = fail ("Semantic error: " ++ f ++ " does not accept " ++ show e)
transform (Lit s) = return (Param s)

readDouble :: (MonadFail m) => String -> m Double
readDouble s =
    case readMaybe s of
        Just d -> return d
        Nothing -> fail ("Unparsable double: " ++ s)
