-- |
-- Module:      Data.Geo.Jord.Angle
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with angles representing latitudes, longitude and bearings.
--
-- In order to use this module you should start with the following imports:
--
-- @
-- import Data.Geo.Jord.Angle (Angle)
-- import qualified Data.Geo.Jord.Angle as Angle
-- @
module Data.Geo.Jord.Angle
    (
    -- * The 'Angle' type
      Angle
    -- * Smart constructors
    , decimalDegrees
    , dms
    , radians
    -- * Calculations
    , arcLength
    , central
    , clockwiseDifference
    , isNegative
    , isWithin
    , negate
    , normalise
    -- * Trigonometric functions
    , asin
    , atan2
    , cos
    , sin
    -- * Accessors
    , getDegrees
    , getArcminutes
    , getArcseconds
    , getArcmilliseconds
    -- * Conversions
    , toDecimalDegrees
    , toRadians
    -- * Read
    , angle
    , read
    -- * Misc
    , add
    , subtract
    , zero
    ) where

import Control.Applicative ((<|>))
import Data.Fixed (mod')
import Prelude hiding (atan2, asin, acos, cos, negate, read, sin, subtract)
import qualified Prelude (atan2, asin, cos, sin)
import Text.ParserCombinators.ReadP (ReadP, char, option, readP_to_S, string)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length (metres, toMetres)
import Data.Geo.Jord.Parser

-- | An angle with a resolution of a microarcsecond.
-- When used as a latitude/longitude this roughly translate to a precision
-- of 0.03 millimetres at the equator.
newtype Angle =
    Angle
        { microarcseconds :: Int
        }
    deriving (Eq)

-- | See 'angle'.
instance Read Angle where
    readsPrec _ = readP_to_S angle

-- | Show 'Angle' as degrees, minutes, seconds and milliseconds - e.g. 154°25'43.5".
instance Show Angle where
    show a =
        s ++
        show d ++
        "°" ++
        show (getArcminutes a) ++
        "'" ++ show (getArcseconds a) ++ "." ++ printf "%03d" (getArcmilliseconds a) ++ "\""
      where
        d = getDegrees a
        s =
            if d == 0 && microarcseconds a < 0
                then "-"
                else ""

instance Ord Angle where
    (<=) (Angle uas1) (Angle uas2) = uas1 <= uas2

-- | Adds 2 angles.
add :: Angle -> Angle -> Angle
add a1 a2 = Angle (microarcseconds a1 + microarcseconds a2)

-- | Subtracts 2 angles.
subtract :: Angle -> Angle -> Angle
subtract a1 a2 = Angle (microarcseconds a1 - microarcseconds a2)

-- | 0 degrees.
zero :: Angle
zero = Angle 0

-- | 'Angle' from given decimal degrees. Any 'Double' is accepted: it must be
-- validated by the call site when representing a latitude or longitude.
decimalDegrees :: Double -> Angle
decimalDegrees dec = Angle (round (dec * 3600000000.0))

-- | 'Angle' from the given degrees, arcminutes and __decimal__ arcseconds.
-- A 'Left' indicates that given arcminutes and/or arcseconds are invalid.
dms :: Int -> Int -> Double -> Either String Angle
dms degs mins secs
    | mins < 0 || mins > 59 = Left ("Invalid arcminutes: " ++ show mins)
    | secs < 0 || secs >= 60 = Left ("Invalid arcseconds: " ++ show secs)
    | otherwise = Right (decimalDegrees d)
  where
    d =
        signed
            (fromIntegral (abs degs) + (fromIntegral mins / 60.0 :: Double) +
             (secs / 3600.0))
            (signum degs)

-- | 'Angle' from the given radians.
radians :: Double -> Angle
radians r = decimalDegrees (r / pi * 180.0)

-- | @arcLength a r@ computes the 'Length' of the arc that subtends the angle @a@ for radius @r@.
arcLength :: Angle -> Length -> Length
arcLength a r = Length.metres (Length.toMetres r * toRadians a)

-- | @central l r@ computes the central 'Angle' from the arc length @l@ and radius @r@.
central :: Length -> Length -> Angle
central s r = radians (Length.toMetres s / Length.toMetres r)

-- | @clockwiseDifference f s@ computes the angle between given angles, rotating clockwise.
clockwiseDifference :: Angle -> Angle -> Angle
clockwiseDifference f s = decimalDegrees d
  where
    d = cd (toDecimalDegrees f) (toDecimalDegrees s)

cd :: Double -> Double -> Double
cd d1 d2
  | d2 < d1 = cd d1 (d2 + 360.0)
  | otherwise = d2 - d1

-- | Returns the given 'Angle' negated.
negate :: Angle -> Angle
negate (Angle millis) = Angle (-millis)

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

-- | @atan2 y x@ computes the 'Angle' (from the positive x-axis) of the vector from the origin to the point (x,y).
atan2 :: Double -> Double -> Angle
atan2 y x = radians (Prelude.atan2 y x)

-- | @asin a@ computes arc sine of @a@.
asin :: Double -> Angle
asin a = radians (Prelude.asin a)

-- | @cos a@ returns the cosinus of @a@.
cos :: Angle -> Double
cos a = Prelude.cos (toRadians a)

-- | @sin a@ returns the sinus of @a@.
sin :: Angle -> Double
sin a = Prelude.sin (toRadians a)

-- | degrees to radians.
toRadians :: Angle -> Double
toRadians a = toDecimalDegrees a * pi / 180.0

-- | Converts the given 'Angle' to decimal degrees.
toDecimalDegrees :: Angle -> Double
toDecimalDegrees (Angle uas) = fromIntegral uas / 3600000000.0

-- | @getDegrees a@ returns the degree component of @a@.
getDegrees :: Angle -> Int
getDegrees a = signed (field a 3600000000.0 360.0) (signum (microarcseconds a))

-- | @getArcminutes a@ returns the arcminute component of @a@.
getArcminutes :: Angle -> Int
getArcminutes a = field a 60000000.0 60.0

-- | @getArcseconds a@ returns the arcsecond component of @a@.
getArcseconds :: Angle -> Int
getArcseconds a = field a 1000000.0 60.0

-- | @getArcmilliseconds a@ returns the arcmilliseconds component of @a@.
getArcmilliseconds :: Angle -> Int
getArcmilliseconds a = field a 1000.0 1000.0

field :: Angle -> Double -> Double -> Int
field (Angle uas) divisor modulo =
    truncate (mod' (fromIntegral (abs uas) / divisor) modulo) :: Int

signed :: (Num a, Num b, Ord b) => a -> b -> a
signed n s
    | s < 0 = -n
    | otherwise = n

-- | Parses and returns an 'Angle'.
--
-- Supported formats:
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
angle :: ReadP Angle
angle = degsMinsSecs <|> decimal

-- | Reads an 'Angle' from the given string using 'angle'.
read :: String -> Maybe Angle
read s = readMaybe s :: (Maybe Angle)

-- | Parses DMS.MS and returns an 'Angle'.
degsMinsSecs :: ReadP Angle
degsMinsSecs = do
    d' <- fmap fromIntegral integer
    degSymbol
    (m', s') <- option (0, 0.0) (minsSecs <|> minsOnly)
    case dms d' m' s' of
        Left err -> fail err
        Right a -> return a

-- | Parses arcminutes and arcseconds.
minsSecs :: ReadP (Int, Double)
minsSecs = do
    m' <- natural
    minSymbol
    s' <- number
    secSymbol
    return (m', s')

-- | Parses minutes.
minsOnly :: ReadP (Int, Double)
minsOnly = do
    m' <- natural
    minSymbol
    return (m', 0.0)

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
