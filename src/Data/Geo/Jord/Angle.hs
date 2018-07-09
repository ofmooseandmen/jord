-- |
-- Module:      Data.Geo.Jord.Angle
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with angles representing latitudes, longitude and bearings.
-- Angles are internally stored as degrees, minutes, seconds and whole milliseconds.
-- When used as a latitude/longitude this roughly translate to a precision
-- of 30 millimetres at the equator (which should be good enough for geodesic calculations).
--
module Data.Geo.Jord.Angle
    ( Angle(degrees, minutes, seconds, milliseconds)
    , angle
    , angleParser
    , arcLength
    , atan2'
    , central
    , cos'
    , decimalDegrees
    , isNegative
    , isWithin
    , neg
    , normalise
    , readAngle
    , readAngleE
    , readAngleF
    , sin'
    , toDecimalDegrees
    ) where

import Control.Applicative
import Control.Monad.Fail
import Data.Char
import Data.Fixed
import Data.Geo.Jord.Length
import Data.Geo.Jord.Parse
import Data.Geo.Jord.Quantity
import Prelude hiding (fail, length)
import Text.ParserCombinators.ReadP
import Text.Read hiding (get, look, pfail)

-- | An angle.
data Angle = Angle
    { degrees :: Int
    , minutes :: Int
    , seconds :: Int
    , milliseconds :: Int
    } deriving (Eq)

-- | See 'readAngle'.
instance Read Angle where
    readsPrec _ = readP_to_S angleParser

-- | Angle is shown degrees, minutes, seconds and milliseconds - e.g. 154°25'43.5".
instance Show Angle where
    show (Angle degs mins secs millis) =
        show degs ++ "°" ++ show mins ++ "'" ++ show secs ++ "." ++ show millis ++ "\""

-- | Add/Subtract 'Angle'.
instance Quantity Angle where
    add a b = decimalDegrees (toDecimalDegrees a + toDecimalDegrees b)
    sub a b = decimalDegrees (toDecimalDegrees a - toDecimalDegrees b)

-- | Returns an 'Angle' from the given given degrees, minutes, seconds and milliseconds.
-- 'fail's if minutes, seconds and/or milliseconds are invalid.
angle :: (MonadFail m) => Int -> Int -> Int -> Int -> m Angle
angle degs mins secs millis
    | mins < 0 || mins > 59 = fail ("Invalid minutes: " ++ show mins)
    | secs < 0 || secs >= 60 = fail ("Invalid seconds: " ++ show secs)
    | millis < 0 || millis >= 1000 = fail ("Invalid milliseconds: " ++ show millis)
    | otherwise = return (Angle degs mins secs millis)

-- | Parses and returns an 'Angle'.
angleParser :: ReadP Angle
angleParser = dms <|> decimal

-- | Computes the length of the arc that subtends the given 'Angle' for the given radius.
arcLength :: Angle -> Length -> Length
arcLength a r = metres (toMetres r * toRadians a)

-- | @'atan2'' y x@ computes the 'Angle' (from the positive x-axis) of the vector from the origin to the point (x,y).
atan2' :: Double -> Double -> Angle
atan2' y x = fromRadians (atan2 y x)

-- | Computes the central 'Angle' from the given arc length and radius.
central :: Length -> Length -> Angle
central s r = fromRadians (toMetres s / toMetres r)

-- | cosinus of the given 'Angle'.
cos' :: Angle -> Double
cos' a = cos (toRadians a)

-- | Returns an 'Angle' from given decimal degrees.
decimalDegrees :: Double -> Angle
decimalDegrees dec =
    case angle degs mins secs millis of
        Nothing -> error ("degs: " ++ show degs ++ ", mins: " ++ show mins ++ ", secs: "
                         ++ show secs ++ ", millis: " ++ show millis)
        Just a -> a
  where
    sign = signum dec
    aDec = abs dec
    rDec = fromIntegral (round (aDec * 3600000) :: Int) / 3600000
    aDegs = truncate rDec :: Int -- whole degrees
    degs = if sign < 0
        then (-aDegs)
        else aDegs
    decM = (rDec - fromIntegral aDegs) * 60.0 -- decimal minutes
    mins = truncate decM :: Int -- whole minutes
    decS = (decM - fromIntegral mins) * 60.0 -- decimal seconds
    secs = truncate decS :: Int -- whole seconds
    millis = truncate ((decS - fromIntegral secs) * 1000.0) :: Int -- whole milliseconds

-- | Returns the given 'Angle' negated.
neg :: Angle -> Angle
neg (Angle degs mins secs millis) = Angle (-degs) mins secs millis

-- | normalise given 'Angle' to [0, @n@].
normalise :: Angle -> Int -> Angle
normalise (Angle degs mins secs millis) n = Angle ndegs mins secs millis
   where ndegs = mod' (degs + n) 360

-- | Is given 'Angle' < 0?
isNegative :: Angle -> Bool
isNegative (Angle degs _ _ _) = degs < 0

-- | Is given 'Angle' within range [@low@..@high@] inclusive?
isWithin :: Angle -> Double -> Double -> Bool
isWithin a low high = dec >= low && dec <= high
  where
    dec = toDecimalDegrees a

-- | Obtains a 'Angle' from the given string formatted as either:
--
--     * d°m's.ms'' - e.g. 55°36'21.3'', where minutes, seconds and milliseconds are optional.
--
--     * decimal° - e.g. 55.6050° or -133°
--
-- Symbols used as separator can be any combination of non alphanumeric characters (expect .).
--
-- This simply calls @read s :: Angle@ so 'error' should be handled at the call site.
--
readAngle :: String -> Angle
readAngle s = read s :: Angle

-- | Same as 'readAngle' but returns an 'Either'.
readAngleE :: String -> Either String Angle
readAngleE s =
    case readMaybe s of
        Nothing -> Left ("couldn't read angle " ++ s)
        Just a -> Right a

-- | Same as 'readAngle' but returns a 'MonadFail'.
readAngleF :: (MonadFail m) => String -> m Angle
readAngleF s =
    let p = readAngleE s
     in case p of
            Left e -> fail e
            Right l -> return l

-- | sinus of the given 'Angle'.
sin' :: Angle -> Double
sin' a = sin (toRadians a)

-- | Converts the given 'Angle' into decimal degrees.
toDecimalDegrees :: Angle -> Double
toDecimalDegrees (Angle degs mins secs millis) =
    sign * (fromIntegral (abs degs) + fromIntegral mins / 60.0 + fromIntegral secs / 3600.0 +
    fromIntegral millis / 3600000.0)
    where
      sign = fromIntegral (signum degs)

-- Private functions.
-- | radians to degrees.
fromRadians :: Double -> Angle
fromRadians r = decimalDegrees (r / pi * 180.0)

-- | degrees to radians.
toRadians :: Angle -> Double
toRadians a = toDecimalDegrees a * pi / 180.0

-- | Parses DMS.MS and returns an 'Angle'.
dms :: ReadP Angle
dms = do
    d' <- fmap fromIntegral integer
    _ <- symbol
    (m', s', ms') <- option (0, 0, 0) (ms <|> m)
    angle d' m' s' ms'

-- | Parses minutes, seconds with optionally milliseconds.
ms :: ReadP (Int, Int, Int)
ms = do
    m' <- natural
    _ <- symbol
    s' <- natural
    ms'  <- option 0 (char '.' >> natural)
    _ <- symbol
    return (m', s', ms')

-- | Parses minutes.
m :: ReadP (Int, Int, Int)
m = do
    m' <- natural
    _ <- symbol
    return (m', 0, 0)

-- | Parses decimal degrees.
decimal :: ReadP Angle
decimal = do
    d <- double
    _ <- symbol
    return (decimalDegrees d)

-- | Parses all characters expects upper case letters, digits and '.'.
-- TODO
symbol :: ReadP String
symbol = munch1 (\c -> not (isAlphaNum c) && c /= '.')
