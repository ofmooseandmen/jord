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
import Data.Either (rights)
import Data.Geo.Jord
import Data.List hiding (delete, insert, lookup)
import Data.Maybe
import Prelude hiding (fail, lookup)
import Text.ParserCombinators.ReadP
import Text.Read (readEither, readMaybe)

-- | A value accepted and returned by 'eval'.
data Value
    = Ang Angle -- ^ angle
    | Bool Bool -- ^ boolean
    | Cpa (Cpa (AngularPosition NVector)) -- ^ CPA
    | Dlt Delta -- ^ delta
    | Dur Duration -- ^ duration
    | Double Double -- ^ double
    | Ep EcefPosition -- ^ ECEF position
    | Em Earth -- ^ earth model
    | FrmB Angle
           Angle
           Angle -- ^ yaw, pitch and roll of Body frame
    | FrmL Angle -- ^ wander azimuth of Local frame
    | FrmN -- ^ North, east, down frame
    | Len Length -- ^ length
    | Gc GreatCircle -- ^ great circle
    | Gp (AngularPosition LatLong) -- ^ latitude, longitude and height
    | Ned Ned -- ^ north east down
    | Np (AngularPosition NVector) -- ^ n-vector and height
    | Spd Speed -- ^ speed
    | Trk (Track (AngularPosition NVector)) -- ^ track
    | Vals [Value] -- array of values

-- | show value.
instance Show Value where
    show (Ang a) = "angle: " ++ showAng a
    show (Bool b) = show b
    show (Cpa c) =
        "closest point of approach:" ++
        "\n      time    : " ++
        show (cpaTime c) ++
        "\n      distance: " ++
        showLen (cpaDistance c) ++
        "\n      pos1    : " ++
        showLl (fromNVector . cpaPosition1 $ c :: LatLong) ++
        "\n      pos2    : " ++ showLl (fromNVector . cpaPosition2 $ c :: LatLong)
    show (Dlt d) =
        "Delta:" ++
        "\n      x: " ++
        showLen (dx d) ++ "\n      y: " ++ showLen (dy d) ++ "\n      z: " ++ showLen (dz d)
    show (Dur d) = "duration: " ++ show d
    show (Double d) = show d
    show (Em m) = "Earth model: " ++ show m
    show (Ep p) =
        "ECEF:" ++
        "\n      x: " ++
        showLen (ex p) ++ "\n      y: " ++ showLen (ey p) ++ "\n      z: " ++ showLen (ez p)
    show (FrmB y p r) =
        "Body (vehicle) frame:" ++
        "\n      yaw  : " ++
        showAng y ++ "\n      pitch: " ++ showAng p ++ "\n      roll : " ++ showAng r
    show (FrmL w) = "Local frame:" ++ "\n      wander azimuth: " ++ showAng w
    show FrmN = "North, East, Down frame"
    show (Len l) = "length: " ++ showLen l
    show (Gc gc) = "great circle: " ++ show gc
    show (Gp g) = "latlong: " ++ showLl ll ++ "\n      height : " ++ showLen h
      where
        ll = pos g
        h = height g
    show (Ned d) =
        "NED:" ++
        "\n      north: " ++
        showLen (north d) ++
        "\n      east : " ++ showLen (east d) ++ "\n      down : " ++ showLen (down d)
    show (Np nv) =
        "n-vector: " ++
        show x ++ ", " ++ show y ++ ", " ++ show z ++ "\n      height  : " ++ showLen h
      where
        v = vec (pos nv)
        x = vx v
        y = vy v
        z = vz v
        h = height nv
    show (Trk t) =
        "track:" ++
        "\n      position: " ++
        showLl (fromNVector . trackPos $ t :: LatLong) ++
        "\n      height  : " ++
        showLen (height . trackPos $ t) ++
        "\n      bearing : " ++
        showAng (trackBearing t) ++ "\n      speed   : " ++ showSpd (trackSpeed t)
    show (Spd s) = "speed: " ++ showSpd s
    show (Vals []) = "empty"
    show (Vals vs) = "\n  " ++ intercalate "\n\n  " (map show vs)

showAng :: Angle -> String
showAng a = show a ++ " (" ++ show (toDecimalDegrees a) ++ ")"

showLl :: LatLong -> String
showLl ll =
    show ll ++
    " (" ++
    show (toDecimalDegrees (latitude ll)) ++ ", " ++ show (toDecimalDegrees (longitude ll)) ++ ")"

showLen :: Length -> String
showLen l =
    show (toMetres l) ++
    "m <-> " ++
    show (toKilometres l) ++
    "km <-> " ++ show (toNauticalMiles l) ++ "nm <-> " ++ show (toFeet l) ++ "ft"

showSpd :: Speed -> String
showSpd s =
    show (toKilometresPerHour s) ++
    "km/h <-> " ++
    show (toMetresPerSecond s) ++
    "m/s <-> " ++
    show (toKnots s) ++
    "kt <-> " ++ show (toMilesPerHour s) ++ "mph <-> " ++ show (toFeetPerSecond s) ++ "ft/s"

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
--     length = eval "surfaceDistance (antipode 54N154E) 54S154W" vault -- Right Len
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
eval :: String -> Vault -> Result
eval s r =
    case expr s of
        Left err -> Left err
        Right (rvec, expr') -> convert (evalExpr expr' r) rvec

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
    fmap (expectVec ts, ) (transform ast)

expectVec :: [Token] -> Bool
expectVec (_:Func "toNVector":_) = True
expectVec _ = False

evalExpr :: Expr -> Vault -> Result
evalExpr (Param p) vault =
    case lookup p vault of
        Just (Gp geo) -> Right (Np (toNVector geo))
        Just v -> Right v
        Nothing -> tryRead p
evalExpr (Antipode a) vault =
    case evalExpr a vault of
        (Right (Np p)) -> Right (Np (antipode p))
        r -> Left ("Call error: antipode " ++ showErr [r])
evalExpr (ClosestPointOfApproach a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Trk t1), Right (Trk t2)] ->
            maybe (Left "closest point of approach in the past") (Right . Cpa) (cpa84 t1 t2)
        r -> Left ("Call error: cpa " ++ showErr r)
evalExpr (CrossTrackDistance a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Np p), Right (Gc gc)] -> Right (Len (crossTrackDistance84 p gc))
        r -> Left ("Call error: crossTrackDistance " ++ showErr r)
evalExpr (DeltaBetween a b c d) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault, evalEarth d] of
        [Right (Np p1), Right (Np p2), Right (FrmB y p r), Right (Em m)] ->
            Right (Dlt (deltaBetween p1 p2 (frameB y p r) m))
        [Right (Np p1), Right (Np p2), Right (FrmL w), Right (Em m)] ->
            Right (Dlt (deltaBetween p1 p2 (frameL w) m))
        [Right (Np p1), Right (Np p2), Right FrmN, Right (Em m)] ->
            Right (Dlt (deltaBetween p1 p2 frameN m))
        r -> Left ("Call error: deltaBetween " ++ showErr r)
evalExpr (DeltaV a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault] of
        [Right (Len x), Right (Len y), Right (Len z)] -> Right (Dlt (delta x y z))
        [Right (Double x), Right (Double y), Right (Double z)] -> Right (Dlt (deltaMetres x y z))
        r -> Left ("Call error: delta " ++ showErr r)
evalExpr (Destination a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault] of
        [Right (Np p), Right (Ang a'), Right (Len l)] -> Right (Np (destination84 p a' l))
        [Right (Np p), Right (Double a'), Right (Len l)] ->
            Right (Np (destination84 p (decimalDegrees a') l))
        r -> Left ("Call error: destination " ++ showErr r)
evalExpr (Ecef a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault] of
        [Right (Len x), Right (Len y), Right (Len z)] -> Right (Ep (ecef x y z))
        [Right (Double x), Right (Double y), Right (Double z)] -> Right (Ep (ecefMetres x y z))
        r -> Left ("Call error: ecef " ++ showErr r)
evalExpr (FrameB a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault] of
        [Right (Ang a'), Right (Ang b'), Right (Ang c')] -> Right (FrmB a' b' c')
        r -> Left ("Call error: frameB " ++ showErr r)
evalExpr (FrameL a) vault =
    case evalExpr a vault of
        (Right (Ang a')) -> Right (FrmL a')
        r -> Left ("Call error: frameL " ++ showErr [r])
evalExpr FrameN _ = Right FrmN
evalExpr (FromEcef a b) vault =
    case [evalExpr a vault, evalEarth b] of
        [Right (Ep p), Right (Em m)] -> Right (Np (fromEcef p m))
        r -> Left ("Call error: fromEcef " ++ showErr r)
evalExpr (FinalBearing a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Np p1), Right (Np p2)] ->
            maybe
                (Left "Call error: finalBearing identical points")
                (Right . Ang)
                (finalBearing p1 p2)
        r -> Left ("Call error: finalBearing " ++ showErr r)
evalExpr (Geo as) vault =
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
        r -> Left ("Call error: geo " ++ showErr r)
  where
    vs = map (`evalExpr` vault) as
evalExpr (GreatCircleE as) vault =
    case fmap (`evalExpr` vault) as of
        [Right (Np p1), Right (Np p2)] -> bimap id Gc (greatCircleE (p1, p2))
        [Right (Np p), Right (Ang a')] -> bimap id Gc (greatCircleE (p, a'))
        [Right (Trk t)] -> bimap id Gc (greatCircleE t)
        r -> Left ("Call error: greatCircle " ++ showErr r)
evalExpr (InitialBearing a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Np p1), Right (Np p2)] ->
            maybe
                (Left "Call error: initialBearing identical points")
                (Right . Ang)
                (initialBearing p1 p2)
        r -> Left ("Call error: initialBearing " ++ showErr r)
evalExpr (Interpolate a b c) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Np p1), Right (Np p2)] -> Right (Np (interpolate p1 p2 c))
        r -> Left ("Call error: interpolate " ++ showErr r)
evalExpr (Intersections a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Gc gc1), Right (Gc gc2)] ->
            maybe
                (Right (Vals []))
                (\is -> Right (Vals [Np (fst is), Np (snd is)]))
                (intersections gc1 gc2 :: Maybe (AngularPosition NVector, AngularPosition NVector))
        r -> Left ("Call error: intersections " ++ showErr r)
evalExpr (InsideSurface as) vault =
    let m = map (`evalExpr` vault) as
        ps = [p | Right (Np p) <- m]
     in if length m == length ps && length ps > 3
            then Right (Bool (insideSurface (head ps) (tail ps)))
            else Left ("Call error: insideSurface " ++ showErr m)
evalExpr (Mean as) vault =
    let m = map (`evalExpr` vault) as
        ps = [p | Right (Np p) <- m]
     in if length m == length ps
            then maybe (Left ("Call error: mean " ++ showErr m)) (Right . Np) (mean ps)
            else Left ("Call error: mean " ++ showErr m)
evalExpr (NedBetween a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalEarth c] of
        [Right (Np p1), Right (Np p2), Right (Em m)] -> Right (Ned (nedBetween p1 p2 m))
        r -> Left ("Call error: nedBetween " ++ showErr r)
evalExpr (NedV a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault] of
        [Right (Len x), Right (Len y), Right (Len z)] -> Right (Ned (ned x y z))
        [Right (Double x), Right (Double y), Right (Double z)] -> Right (Ned (nedMetres x y z))
        r -> Left ("Call error: ned " ++ showErr r)
evalExpr (Position a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Trk t), Right (Dur d)] -> Right (Np (position84 t d))
        r -> Left ("Call error: position " ++ showErr r)
evalExpr (SurfaceDistance a b) vault =
    case [evalExpr a vault, evalExpr b vault] of
        [Right (Np p1), Right (Np p2)] -> Right (Len (surfaceDistance84 p1 p2))
        r -> Left ("Call error: surfaceDistance " ++ showErr r)
evalExpr (Target a b c d) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault, evalEarth d] of
        [Right (Np p0), Right (FrmB y p r), Right (Dlt d'), Right (Em m)] ->
            Right (Np (target p0 (frameB y p r) d' m))
        [Right (Np p0), Right (FrmL w), Right (Dlt d'), Right (Em m)] ->
            Right (Np (target p0 (frameL w) d' m))
        [Right (Np p0), Right FrmN, Right (Dlt d'), Right (Em m)] ->
            Right (Np (target p0 frameN d' m))
        r -> Left ("Call error: target " ++ showErr r)
evalExpr (TargetN a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalEarth c] of
        [Right (Np p0), Right (Ned d), Right (Em m)] -> Right (Np (targetN p0 d m))
        r -> Left ("Call error: targetN " ++ showErr r)
evalExpr (TrackE a b c) vault =
    case [evalExpr a vault, evalExpr b vault, evalExpr c vault] of
        [Right (Np p), Right (Ang b'), Right (Spd s)] -> Right (Trk (Track p b' s))
        r -> Left ("Call error: track " ++ showErr r)
evalExpr (ToEcef a b) vault =
    case [evalExpr a vault, evalEarth b] of
        [Right (Np p), Right (Em m)] -> Right (Ep (toEcef p m))
        r -> Left ("Call error: toEcef " ++ showErr r)
evalExpr (ToNVector a) vault =
    case evalExpr a vault of
        r@(Right (Np _)) -> r
        r -> Left ("Call error: toNVector " ++ showErr [r])

evalEarth :: String -> Result
evalEarth "wgs84" = Right (Em wgs84)
evalEarth "grs80" = Right (Em grs80)
evalEarth "wgs72" = Right (Em wgs72)
evalEarth "s84" = Right (Em s84)
evalEarth "s80" = Right (Em s80)
evalEarth "s72" = Right (Em s72)
evalEarth s = Left s

showErr :: [Result] -> String
showErr rs = " > " ++ intercalate " & " (map (either id show) rs)

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
