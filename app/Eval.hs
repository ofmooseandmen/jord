{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Eval
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for evaluating expressions in textual form.
--
module Eval
    ( Result
    , eval
    , functions
    ) where

import Control.Monad.Fail
import Data.Bifunctor
import Data.Either (rights)
import Data.Geo.Jord
import Data.List (intercalate)
import Data.Maybe
import Prelude hiding (fail, lookup)
import Show
import State
import Text.ParserCombinators.ReadP
import Text.Read (readEither, readMaybe)

-- | 'Either' an error or a 'Value'.
type Result = Either String Value

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
--     state = emptyState
--     angle = eval "finalBearing 54N154E 54S154W" state -- Right Ang
--     length = eval "surfaceDistance (antipode 54N154E) 54S154W" state -- Right Len
--     -- parameter resolution from state
--     a1 = eval "finalBearing 54N154E 54S154W" state
--     state = insert "a1" state
--     a2 = eval "(finalBearing a1 54S154W)" state
-- @
--
-- All returned positions are 'LatLong' by default, to get back a n-vector the
-- expression must be wrapped by 'toNVector'.
--
-- @
--     dest = eval "destination 54°N,154°E 54° 1000m" -- Right Ll
--     dest = eval "toNVector (destination 54°N,154°E 54° 1000m)" -- Right Np
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
eval :: String -> State -> Result
eval st state =
    case expr st of
        Left err -> Left err
        Right (rvec, expr') -> convert (evalExpr expr' state) rvec

convert :: Result -> Bool -> Result
convert r True = r
convert r False =
    case r of
        Right v@(Np _) -> Right (toGeo v)
        Right (Vals vs) -> Right (Vals (map toGeo vs))
        oth -> oth

toGeo :: Value -> Value
toGeo (Np v) = Gp (fromNVector v)
toGeo val = val

-- | All supported functions.
functions :: [String]
functions =
    [ "antipode"
    , "crossTrackDistance"
    , "cpa"
    , "delta"
    , "deltaBetween"
    , "destination"
    , "ecef"
    , "frameB"
    , "frameL"
    , "frameN"
    , "finalBearing"
    , "fromEcef"
    , "geo"
    , "greatCircle"
    , "initialBearing"
    , "intercept"
    , "interpolate"
    , "intersections"
    , "insideSurface"
    , "mean"
    , "ned"
    , "nedBetween"
    , "position"
    , "surfaceDistance"
    , "target"
    , "targetN"
    , "track"
    , "toEcef"
    , "toNVector"
    ]

expr :: (MonadFail m) => String -> m (Bool, Expr)
expr s = do
    ts <- tokenise s
    ast <- parse ts
    fmap (expectVec ts, ) (transform ast)

expectVec :: [Token] -> Bool
expectVec (_:Func "toNVector":_) = True
expectVec _ = False

evalExpr :: Expr -> State -> Result
evalExpr (Param p) state =
    case lookup p state of
        Just (Gp geo) -> Right (Np (toNVector geo))
        Just v -> Right v
        Nothing -> tryRead p
evalExpr (Antipode a) state =
    case evalExpr a state of
        (Right (Np p)) -> Right (Np (antipode p))
        r -> Left ("Call error: antipode " ++ showErr [r] state)
evalExpr (ClosestPointOfApproach a b) state =
    case [evalExpr a state, evalExpr b state] of
        [Right (Trk t1), Right (Trk t2)] ->
            maybe (Left "closest point of approach in the past") (Right . Cpa) (cpa84 t1 t2)
        r -> Left ("Call error: cpa " ++ showErr r state)
evalExpr (CrossTrackDistance a b) state =
    case [evalExpr a state, evalExpr b state] of
        [Right (Np p), Right (Gc gc)] -> Right (Len (crossTrackDistance84 p gc))
        r -> Left ("Call error: crossTrackDistance " ++ showErr r state)
evalExpr (DeltaBetween a b c d) state =
    case [evalExpr a state, evalExpr b state, evalExpr c state, evalEarth d] of
        [Right (Np p1), Right (Np p2), Right (FrmB y p r), Right (Em m)] ->
            Right (Dlt (deltaBetween p1 p2 (frameB y p r) m))
        [Right (Np p1), Right (Np p2), Right (FrmL w), Right (Em m)] ->
            Right (Dlt (deltaBetween p1 p2 (frameL w) m))
        [Right (Np p1), Right (Np p2), Right FrmN, Right (Em m)] ->
            Right (Dlt (deltaBetween p1 p2 frameN m))
        r -> Left ("Call error: deltaBetween " ++ showErr r state)
evalExpr (DeltaV a b c) state =
    case [evalExpr a state, evalExpr b state, evalExpr c state] of
        [Right (Len x), Right (Len y), Right (Len z)] -> Right (Dlt (delta x y z))
        [Right (Double x), Right (Double y), Right (Double z)] -> Right (Dlt (deltaMetres x y z))
        r -> Left ("Call error: delta " ++ showErr r state)
evalExpr (Destination a b c) state =
    case [evalExpr a state, evalExpr b state, evalExpr c state] of
        [Right (Np p), Right (Ang a'), Right (Len l)] -> Right (Np (destination84 p a' l))
        [Right (Np p), Right (Double a'), Right (Len l)] ->
            Right (Np (destination84 p (decimalDegrees a') l))
        r -> Left ("Call error: destination " ++ showErr r state)
evalExpr (Ecef a b c) state =
    case [evalExpr a state, evalExpr b state, evalExpr c state] of
        [Right (Len x), Right (Len y), Right (Len z)] -> Right (Ep (ecef x y z))
        [Right (Double x), Right (Double y), Right (Double z)] -> Right (Ep (ecefMetres x y z))
        r -> Left ("Call error: ecef " ++ showErr r state)
evalExpr (FrameB a b c) state =
    case [evalExpr a state, evalExpr b state, evalExpr c state] of
        [Right (Ang a'), Right (Ang b'), Right (Ang c')] -> Right (FrmB a' b' c')
        r -> Left ("Call error: frameB " ++ showErr r state)
evalExpr (FrameL a) state =
    case evalExpr a state of
        (Right (Ang a')) -> Right (FrmL a')
        r -> Left ("Call error: frameL " ++ showErr [r] state)
evalExpr FrameN _ = Right FrmN
evalExpr (FromEcef a b) state =
    case [evalExpr a state, evalEarth b] of
        [Right (Ep p), Right (Em m)] -> Right (Np (fromEcef p m))
        r -> Left ("Call error: fromEcef " ++ showErr r state)
evalExpr (FinalBearing a b) state =
    case [evalExpr a state, evalExpr b state] of
        [Right (Np p1), Right (Np p2)] ->
            maybe
                (Left "Call error: finalBearing identical points")
                (Right . Ang)
                (finalBearing p1 p2)
        r -> Left ("Call error: finalBearing " ++ showErr r state)
evalExpr (Geo as) state =
    case vs of
        [Right p@(Np _)] -> Right p
        [Right (Np v), Right (Len h)] -> Right (Np (AngularPosition (pos v) h))
        [Right (Double lat), Right (Double lon)] ->
            bimap
                (\e -> "Call error: geo " ++ e)
                (Np . toNVector)
                (decimalLatLongHeightE lat lon zero)
        [Right (Double lat), Right (Double lon), Right (Len h)] ->
            bimap (\e -> "Call error: geo " ++ e) (Np . toNVector) (decimalLatLongHeightE lat lon h)
        [Right (Double lat), Right (Double lon), Right (Double h)] ->
            bimap
                (\e -> "Call error: geo " ++ e)
                (Np . toNVector)
                (decimalLatLongHeightE lat lon (metres h))
        r -> Left ("Call error: geo " ++ showErr r state)
  where
    vs = map (`evalExpr` state) as
evalExpr (GreatCircleE as) state =
    case fmap (`evalExpr` state) as of
        [Right (Np p1), Right (Np p2)] -> bimap id Gc (greatCircleE (p1, p2))
        [Right (Np p), Right (Ang a')] -> bimap id Gc (greatCircleE (p, a'))
        [Right (Trk t)] -> bimap id Gc (greatCircleE t)
        r -> Left ("Call error: greatCircle " ++ showErr r state)
evalExpr (InitialBearing a b) state =
    case [evalExpr a state, evalExpr b state] of
        [Right (Np p1), Right (Np p2)] ->
            maybe
                (Left "Call error: initialBearing identical points")
                (Right . Ang)
                (initialBearing p1 p2)
        r -> Left ("Call error: initialBearing " ++ showErr r state)
evalExpr (Intercept as) state =
    case fmap (`evalExpr` state) as of
        [Right (Trk t), Right (Np i)] ->
            maybe (Left "undefined minimum speed intercept") (Right . Intp) (intercept84 t i)
        [Right (Trk t), Right (Np i), Right (Spd s)] ->
            maybe (Left "undefined time to intercept") (Right . Intp) (interceptBySpeed84 t i s)
        [Right (Trk t), Right (Np i), Right (Dur d)] ->
            maybe (Left "undefined speed to intercept") (Right . Intp) (interceptByTime84 t i d)
        r -> Left ("Call error: intercept " ++ showErr r state)
evalExpr (Interpolate a b c) state =
    case [evalExpr a state, evalExpr b state] of
        [Right (Np p1), Right (Np p2)] -> Right (Np (interpolate p1 p2 c))
        r -> Left ("Call error: interpolate " ++ showErr r state)
evalExpr (Intersections a b) state =
    case [evalExpr a state, evalExpr b state] of
        [Right (Gc gc1), Right (Gc gc2)] ->
            maybe
                (Right (Vals []))
                (\is -> Right (Vals [Np (fst is), Np (snd is)]))
                (intersections gc1 gc2 :: Maybe (AngularPosition NVector, AngularPosition NVector))
        r -> Left ("Call error: intersections " ++ showErr r state)
evalExpr (InsideSurface as) state =
    let m = map (`evalExpr` state) as
        ps = [p | Right (Np p) <- m]
     in if length m == length ps && length ps > 3
            then Right (Bool (insideSurface (head ps) (tail ps)))
            else Left ("Call error: insideSurface " ++ showErr m state)
evalExpr (Mean as) state =
    let m = map (`evalExpr` state) as
        ps = [p | Right (Np p) <- m]
     in if length m == length ps
            then maybe (Left ("Call error: mean " ++ showErr m state)) (Right . Np) (mean ps)
            else Left ("Call error: mean " ++ showErr m state)
evalExpr (NedBetween a b c) state =
    case [evalExpr a state, evalExpr b state, evalEarth c] of
        [Right (Np p1), Right (Np p2), Right (Em m)] -> Right (Ned (nedBetween p1 p2 m))
        r -> Left ("Call error: nedBetween " ++ showErr r state)
evalExpr (NedV a b c) state =
    case [evalExpr a state, evalExpr b state, evalExpr c state] of
        [Right (Len x), Right (Len y), Right (Len z)] -> Right (Ned (ned x y z))
        [Right (Double x), Right (Double y), Right (Double z)] -> Right (Ned (nedMetres x y z))
        r -> Left ("Call error: ned " ++ showErr r state)
evalExpr (Position a b) state =
    case [evalExpr a state, evalExpr b state] of
        [Right (Trk t), Right (Dur d)] -> Right (Np (position84 t d))
        r -> Left ("Call error: position " ++ showErr r state)
evalExpr (SurfaceDistance a b) state =
    case [evalExpr a state, evalExpr b state] of
        [Right (Np p1), Right (Np p2)] -> Right (Len (surfaceDistance84 p1 p2))
        r -> Left ("Call error: surfaceDistance " ++ showErr r state)
evalExpr (Target a b c d) state =
    case [evalExpr a state, evalExpr b state, evalExpr c state, evalEarth d] of
        [Right (Np p0), Right (FrmB y p r), Right (Dlt d'), Right (Em m)] ->
            Right (Np (target p0 (frameB y p r) d' m))
        [Right (Np p0), Right (FrmL w), Right (Dlt d'), Right (Em m)] ->
            Right (Np (target p0 (frameL w) d' m))
        [Right (Np p0), Right FrmN, Right (Dlt d'), Right (Em m)] ->
            Right (Np (target p0 frameN d' m))
        r -> Left ("Call error: target " ++ showErr r state)
evalExpr (TargetN a b c) state =
    case [evalExpr a state, evalExpr b state, evalEarth c] of
        [Right (Np p0), Right (Ned d), Right (Em m)] -> Right (Np (targetN p0 d m))
        r -> Left ("Call error: targetN " ++ showErr r state)
evalExpr (TrackE a b c) state =
    case [evalExpr a state, evalExpr b state, evalExpr c state] of
        [Right (Np p), Right (Ang b'), Right (Spd s)] -> Right (Trk (Track p b' s))
        r -> Left ("Call error: track " ++ showErr r state)
evalExpr (ToEcef a b) state =
    case [evalExpr a state, evalEarth b] of
        [Right (Np p), Right (Em m)] -> Right (Ep (toEcef p m))
        r -> Left ("Call error: toEcef " ++ showErr r state)
evalExpr (ToNVector a) state =
    case evalExpr a state of
        r@(Right (Np _)) -> r
        r -> Left ("Call error: toNVector " ++ showErr [r] state)

evalEarth :: String -> Result
evalEarth "wgs84" = Right (Em wgs84)
evalEarth "grs80" = Right (Em grs80)
evalEarth "wgs72" = Right (Em wgs72)
evalEarth "s84" = Right (Em s84)
evalEarth "s80" = Right (Em s80)
evalEarth "s72" = Right (Em s72)
evalEarth s = Left s

showErr :: [Result] -> State -> String
showErr rs s = " > " ++ intercalate " & " (map (either id (\r -> showV r s)) rs)

tryRead :: String -> Result
tryRead s
    | null r = Left ("couldn't read " ++ s)
    | otherwise = Right (head r)
  where
    r =
        rights
            (map ($ s)
                 [ readE readAngleE Ang
                 , readE readLengthE Len
                 , readE readSpeedE Spd
                 , readE readDurationE Dur
                 , readE readLatLongE (\ll -> Np (toNVector (AngularPosition ll zero)))
                 , readE readEither Double
                 ])

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
    | ClosestPointOfApproach Expr
                             Expr
    | CrossTrackDistance Expr
                         Expr
    | DeltaBetween Expr
                   Expr
                   Expr
                   String
    | DeltaV Expr
             Expr
             Expr
    | Destination Expr
                  Expr
                  Expr
    | Ecef Expr
           Expr
           Expr
    | FrameB Expr
             Expr
             Expr
    | FrameL Expr
    | FrameN
    | FinalBearing Expr
                   Expr
    | FromEcef Expr
               String
    | Geo [Expr]
    | GreatCircleE [Expr]
    | InitialBearing Expr
                     Expr
    | Intercept [Expr]
    | Interpolate Expr
                  Expr
                  Double
    | Intersections Expr
                    Expr
    | InsideSurface [Expr]
    | Mean [Expr]
    | NedBetween Expr
                 Expr
                 String
    | NedV Expr
           Expr
           Expr
    | Position Expr
               Expr
    | SurfaceDistance Expr
                      Expr
    | Target Expr
             Expr
             Expr
             String
    | TargetN Expr
              Expr
              String
    | TrackE Expr
             Expr
             Expr
    | ToEcef Expr
             String
    | ToNVector Expr
    deriving (Show)

transform :: (MonadFail m) => Ast -> m Expr
transform (Call "antipode" [e]) = fmap Antipode (transform e)
transform (Call "cpa" [e1, e2]) = do
    t1 <- transform e1
    t2 <- transform e2
    return (ClosestPointOfApproach t1 t2)
transform (Call "crossTrackDistance" [e1, e2]) = do
    p <- transform e1
    gc <- transform e2
    return (CrossTrackDistance p gc)
transform (Call "delta" [e1, e2, e3]) = do
    p1 <- transform e1
    p2 <- transform e2
    p3 <- transform e3
    return (DeltaV p1 p2 p3)
transform (Call "deltaBetween" [e1, e2, e3]) = do
    p1 <- transform e1
    p2 <- transform e2
    f <- transform e3
    return (DeltaBetween p1 p2 f "wgs84")
transform (Call "deltaBetween" [e1, e2, e3, Lit s]) = do
    p1 <- transform e1
    p2 <- transform e2
    f <- transform e3
    return (DeltaBetween p1 p2 f s)
transform (Call "destination" [e1, e2, e3]) = do
    p1 <- transform e1
    p2 <- transform e2
    p3 <- transform e3
    return (Destination p1 p2 p3)
transform (Call "ecef" [e1, e2, e3]) = do
    p1 <- transform e1
    p2 <- transform e2
    p3 <- transform e3
    return (Ecef p1 p2 p3)
transform (Call "frameB" [e1, e2, e3]) = do
    p1 <- transform e1
    p2 <- transform e2
    p3 <- transform e3
    return (FrameB p1 p2 p3)
transform (Call "frameL" [e]) = fmap FrameL (transform e)
transform (Call "frameN" []) = return FrameN
transform (Call "fromEcef" [e]) = do
    p <- transform e
    return (FromEcef p "wgs84")
transform (Call "fromEcef" [e, Lit s]) = do
    p <- transform e
    return (FromEcef p s)
transform (Call "finalBearing" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (FinalBearing p1 p2)
transform (Call "geo" e) = do
    ps <- mapM transform e
    return (Geo ps)
transform (Call "greatCircle" e) = do
    ps <- mapM transform e
    return (GreatCircleE ps)
transform (Call "initialBearing" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (InitialBearing p1 p2)
transform (Call "intercept" e) = do
    ps <- mapM transform e
    return (Intercept ps)
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
transform (Call "mean" e) = do
    ps <- mapM transform e
    return (Mean ps)
transform (Call "ned" [e1, e2, e3]) = do
    p1 <- transform e1
    p2 <- transform e2
    p3 <- transform e3
    return (NedV p1 p2 p3)
transform (Call "nedBetween" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (NedBetween p1 p2 "wgs84")
transform (Call "nedBetween" [e1, e2, Lit s]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (NedBetween p1 p2 s)
transform (Call "position" [e1, e2]) = do
    t <- transform e1
    d <- transform e2
    return (Position t d)
transform (Call "surfaceDistance" [e1, e2]) = do
    p1 <- transform e1
    p2 <- transform e2
    return (SurfaceDistance p1 p2)
transform (Call "target" [e1, e2, e3]) = do
    p0 <- transform e1
    f <- transform e2
    d <- transform e3
    return (Target p0 f d "wgs84")
transform (Call "target" [e1, e2, e3, Lit s]) = do
    p0 <- transform e1
    f <- transform e2
    d <- transform e3
    return (Target p0 f d s)
transform (Call "targetN" [e1, e2]) = do
    p0 <- transform e1
    d <- transform e2
    return (TargetN p0 d "wgs84")
transform (Call "targetN" [e1, e2, Lit s]) = do
    p0 <- transform e1
    d <- transform e2
    return (TargetN p0 d s)
transform (Call "track" [e1, e2, e3]) = do
    p0 <- transform e1
    b <- transform e2
    s <- transform e3
    return (TrackE p0 b s)
transform (Call "toEcef" [e]) = do
    p <- transform e
    return (ToEcef p "wgs84")
transform (Call "toEcef" [e, Lit s]) = do
    p <- transform e
    return (ToEcef p s)
transform (Call "toNVector" [e]) = fmap ToNVector (transform e)
transform (Call f e) = fail ("Semantic error: " ++ f ++ " does not accept " ++ show e)
transform (Lit s) = return (Param s)

readDouble :: (MonadFail m) => String -> m Double
readDouble s =
    case readMaybe s of
        Just d -> return d
        Nothing -> fail ("Unparsable double: " ++ s)
