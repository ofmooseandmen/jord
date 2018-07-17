-- |
-- Module:      Data.Geo.Jord.Angle
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with angles representing latitudes, longitude and bearings.
--
module Data.Geo.Jord.Angle
    (
    -- * The 'Angle' type
      Angle
    -- * Smart constructors
    , decimalDegrees
    , dms
    , dmsE
    , dmsF
    -- * Calculations
    , arcLength
    , central
    , isNegative
    , isWithin
    , negate'
    , normalise
    -- * Trigonometric functions
    , atan2'
    , cos'
    , sin'
    -- * Accessors
    , getDegrees
    , getMinutes
    , getSeconds
    , getMilliseconds
    , toDecimalDegrees
    -- * Read
    , angle
    , readAngle
    , readAngleE
    , readAngleF
    ) where

import Control.Applicative
import Control.Monad.Fail
import Data.Fixed
import Data.Geo.Jord.Length
import Data.Geo.Jord.Parse
import Data.Geo.Jord.Quantity
import Data.Maybe
import Prelude hiding (fail, length)
import Text.ParserCombinators.ReadP
import Text.Read hiding (get, look, pfail)

-- | An angle with a resolution of a milliseconds of a degree.
-- When used as a latitude/longitude this roughly translate to a precision
-- of 30 millimetres at the equator.
newtype Angle = Angle
    { milliseconds :: Int
    } deriving (Eq)

-- | See 'readAngle'.
instance Read Angle where
    readsPrec _ = readP_to_S angle

-- | Angle is shown degrees, minutes, seconds and milliseconds - e.g. 154°25'43.5".
instance Show Angle where
    show a =
        show (getDegrees a) ++
        "°" ++
        show (getMinutes a) ++ "'" ++ show (getSeconds a) ++ "." ++ show (getMilliseconds a) ++ "\""

-- | Add/Subtract 'Angle'.
instance Quantity Angle where
    add (Angle millis1) (Angle millis2) = Angle (millis1 + millis2)
    sub (Angle millis1) (Angle millis2) = Angle (millis1 - millis2)
    zero = Angle 0

-- | 'Angle' from given decimal degrees. Any 'Double' is accepted: it must be
-- validated by the call site when used to represent a latitude or longitude.
decimalDegrees :: Double -> Angle
decimalDegrees dec = Angle (round (dec * 3600000.0))

-- | 'Angle' from the given given degrees, minutes, seconds and milliseconds.
-- 'error's if given minutes, seconds and/or milliseconds are invalid.
-- Degrees are not validated and can be any 'Int': they must be validated by the call site
-- when used to represent a latitude or longitude.
dms :: Int -> Int -> Int -> Int -> Angle
dms degs mins secs millis =
    fromMaybe
        (error
             ("Invalid minutes=" ++
              show mins ++ " or seconds=" ++ show secs ++ " or milliseconds=" ++ show millis))
        (dmsF degs mins secs millis)

-- | 'Angle' from the given given degrees, minutes, seconds and milliseconds.
-- A 'Left' indicates that given minutes, seconds and/or milliseconds are invalid.
-- Degrees are not validated and can be any 'Int': they must be validated by the call site
-- when used to represent a latitude or longitude.
dmsE :: Int -> Int -> Int -> Int -> Either String Angle
dmsE degs mins secs millis
    | mins < 0 || mins > 59 = Left ("Invalid minutes: " ++ show mins)
    | secs < 0 || secs >= 60 = Left ("Invalid seconds: " ++ show secs)
    | millis < 0 || millis >= 1000 = Left ("Invalid milliseconds: " ++ show millis)
    | otherwise = Right (decimalDegrees ms)
  where
    ms =
        signed
            (fromIntegral (abs degs) + (fromIntegral mins / 60.0 :: Double) +
             (fromIntegral secs / 3600.0 :: Double) +
             (fromIntegral millis / 3600000.0 :: Double))
            (signum degs)

-- | 'Angle' from the given given degrees, minutes, seconds and milliseconds.
-- 'fail's if given minutes, seconds and/or milliseconds are invalid.
-- Degrees are not validated and can be any 'Int': they must be validated by the call site
-- when used to represent a latitude or longitude.
dmsF :: (MonadFail m) => Int -> Int -> Int -> Int -> m Angle
dmsF degs mins secs millis =
    case e of
        Left err -> fail err
        Right a -> return a
  where
    e = dmsE degs mins secs millis

-- | @arcLength a r@ computes the 'Length' of the arc that subtends the angle @a@ for radius @r@.
arcLength :: Angle -> Length -> Length
arcLength a r = metres (toMetres r * toRadians a)

-- | @central l r@ computes the central 'Angle' from the arc length @l@ and radius @r@.
central :: Length -> Length -> Angle
central s r = fromRadians (toMetres s / toMetres r)

-- | Returns the given 'Angle' negated.
negate' :: Angle -> Angle
negate' (Angle millis) = Angle (-millis)

-- | @normalise a n@ normalises @a@ to [0, @n@].
normalise :: Angle -> Angle -> Angle
normalise a n = decimalDegrees dec
  where
    dec = mod' (toDecimalDegrees a + toDecimalDegrees n) 360.0

-- | Is given 'Angle' < 0?
isNegative :: Angle -> Bool
isNegative (Angle millis) = millis < 0

-- | Is given 'Angle' within range [@low@..@high@] inclusive?
isWithin :: Angle -> Angle -> Angle -> Bool
isWithin (Angle millis) (Angle low) (Angle high) = millis >= low && millis <= high

-- | @atan2' y x@ computes the 'Angle' (from the positive x-axis) of the vector from the origin to the point (x,y).
atan2' :: Double -> Double -> Angle
atan2' y x = fromRadians (atan2 y x)

-- | @cos' a@ returns the cosinus of @a@.
cos' :: Angle -> Double
cos' a = cos (toRadians a)

-- | @sin' a@ returns the sinus of @a@.
sin' :: Angle -> Double
sin' a = sin (toRadians a)

-- | radians to degrees.
fromRadians :: Double -> Angle
fromRadians r = decimalDegrees (r / pi * 180.0)

-- | degrees to radians.
toRadians :: Angle -> Double
toRadians a = toDecimalDegrees a * pi / 180.0

-- | Converts the given 'Angle' to decimal degrees.
toDecimalDegrees :: Angle -> Double
toDecimalDegrees (Angle millis) = fromIntegral millis / 3600000.0

-- | @getDegrees a@ returns the degree component of @a@.
getDegrees :: Angle -> Int
getDegrees a = signed (field a 3600000.0 360.0) (signum (milliseconds a))

-- | @getMinutes a@ returns the minute component of @a@.
getMinutes :: Angle -> Int
getMinutes a = field a 60000.0 60.0

-- | @getSeconds a@ returns the second component of @a@.
getSeconds :: Angle -> Int
getSeconds a = field a 1000.0 60.0

-- | @getMilliseconds a@ returns the milliseconds component of @a@.
getMilliseconds :: Angle -> Int
getMilliseconds (Angle millis) = mod millis 1000

field :: Angle -> Double -> Double -> Int
field (Angle millis) divisor modulo =
    truncate (mod' (fromIntegral (abs millis) / divisor) modulo) :: Int

signed :: (Num a, Num b, Ord b) => a -> b -> a
signed n s
    | s < 0 = -n
    | otherwise = n

-- | Parses and returns an 'Angle'.
angle :: ReadP Angle
angle = degsMinsSecs <|> decimal

-- | Obtains a 'Angle' from the given string formatted as either:
--
--     * d°m′s.ms″ - e.g. 55°36'21.3", where minutes, seconds and milliseconds are optional.
--
--     * decimal° - e.g. 55.6050° or -133°
--
-- Symbols:
--
--     * degree: ° or d
--
--     * minute: ', ′ or m
--
--     * second: ", ″, '' or s
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

-- | Parses DMS.MS and returns an 'Angle'.
degsMinsSecs :: ReadP Angle
degsMinsSecs = do
    d' <- fmap fromIntegral integer
    degSymbol
    (m', s', ms') <- option (0, 0, 0) (minsSecs <|> minsOnly)
    dmsF d' m' s' ms'

-- | Parses minutes, seconds with optionally milliseconds.
minsSecs :: ReadP (Int, Int, Int)
minsSecs = do
    m' <- natural
    minSymbol
    s' <- natural
    ms' <- option 0 (char '.' >> natural)
    secSymbol
    return (m', s', ms')

-- | Parses minutes.
minsOnly :: ReadP (Int, Int, Int)
minsOnly = do
    m' <- natural
    minSymbol
    return (m', 0, 0)

-- | Parses decimal degrees.
decimal :: ReadP Angle
decimal = do
    d <- double
    degSymbol
    return (decimalDegrees d)

-- | skips degree symbol.
degSymbol :: ReadP ()
degSymbol = do
    _ <- char '°' <|> char 'd'
    return ()

-- | skips minute symbol.
minSymbol :: ReadP ()
minSymbol = do
    _ <- char '\'' <|> char '′' <|> char 'm'
    return ()

-- | skips second symbol.
secSymbol :: ReadP ()
secSymbol = do
    _ <- string "\"" <|> string "''" <|> string "″" <|> string "s"
    return ()
