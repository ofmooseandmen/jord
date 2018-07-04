-- |
-- Module:      Data.Geo.Jord.Angle
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with angles in degrees.
--
module Data.Geo.Jord.Angle
    ( Angle(degrees)
    , angle
    , fromDMS
    , ofDegrees
    , ofRadians
    , neg
    , normalise
    , radians
    , readAngle
    , readAngleF
    , readAngleM
    , toDegrees
    , toRadians
    ) where

import Control.Applicative
import Control.Monad.Fail
import Data.Char
import Data.Fixed
import Data.Geo.Jord.Parse
import Data.Geo.Jord.Quantity
import Prelude hiding (fail, length)
import Text.ParserCombinators.ReadP
import Text.Read hiding (get, look, pfail)

-- | An angle - the value is stored in degrees.
newtype Angle = Angle
    { degrees :: Double
    } deriving (Eq)

-- | See 'readAngle'.
instance Read Angle where
    readsPrec _ = readP_to_S angle

-- | Angle is shown degrees, minutes, seconds and milliseconds - e.g. 154°25'43.5".
instance Show Angle where
    show (Angle dec) =
        show degs ++ "°" ++ show mins ++ "'" ++ show secs ++ "." ++ show millis ++ "\""
      where
        degs = truncate dec :: Int -- whole degrees
        decM = (dec - fromIntegral degs) * 60.0 -- decimal minutes
        mins = truncate decM :: Int -- whole minutes
        decS = (decM - fromIntegral mins) * 60.0 -- decimal seconds
        secs = truncate decS :: Int -- whole seconds
        millis = truncate ((decS - fromIntegral secs) * 1000.0) :: Int -- whole milliseconds

-- | Add/Subtract 'Angle'.
instance Quantity Angle where
    add a b = Angle (degrees a + degrees b)
    sub a b = Angle (degrees a - degrees b)

-- | Parses and returns an 'Angle'.
angle :: ReadP Angle
angle = dms <|> decimal

-- | Computes the decimal degrees value from the given degrees, minutes and seconds, fails if minutes or seconds are invalid.
fromDMS :: (MonadFail m) => Int -> Int -> Double -> m Double
fromDMS d' m' s'
    | m' < 0 || m' > 59 = fail ("Invalid minutes: " ++ show m')
    | s' < 0 || s' >= 60.0 = fail ("Invalid seconds: " ++ show s')
    | otherwise = return (fromIntegral d' + fromIntegral m' / 60.0 + s' / 3600.0)

-- | Returns an 'Angle' from given degrees.
ofDegrees :: Double -> Angle
ofDegrees = Angle

-- | Returns an 'Angle' from given radians.
ofRadians :: Double -> Angle
ofRadians r = Angle (toDegrees r)

-- | Returns the given 'Angle' negated.
neg :: Angle -> Angle
neg (Angle d) = Angle (-d)

-- | normalise given 'Angle' to [0, @n@].
normalise :: Angle -> Double -> Angle
normalise a n = Angle (mod' (degrees a + n) 360.0)

-- | Returns the value of the given 'Angle' in radians.
radians :: Angle -> Double
radians a = toRadians (degrees a)

-- | Obtains a 'Angle' from the given string formatted as either:
--
--     * d°m's.ms'' - e.g. 55°36'21.3'', where minutes, seconds and milliseconds are optional.
--
--     * decimal° - e.g. 55.6050° or -133°
--
-- Symbols used as separator can be any combination of non alphanumeric characters (expect .).
--
-- This simply calls:
-- @
--     read s :: Angle
-- @
-- so 'error' should be handled at the call site.
--
readAngle :: String -> Angle
readAngle s = read s :: Angle

-- | Same as 'readAngle' but returns a 'MonadFail'.
readAngleF :: (MonadFail m) => String -> m Angle
readAngleF s =
    let p = readEither s
     in case p of
            Left e -> fail e
            Right l -> return l

-- | Same as 'readAngle' but returns a 'Maybe'.
readAngleM :: String -> Maybe Angle
readAngleM = readMaybe

-- | radians to degrees.
toDegrees :: Double -> Double
toDegrees r = r / pi * 180.0

-- | degrees to radians.
toRadians :: Double -> Double
toRadians d = d * pi / 180.0

-- | Parses DMS.MS and returns an 'Angle'.
dms :: ReadP Angle
dms = do
    d' <- fmap fromIntegral integer
    _ <- symbol
    (m', s') <- option (0, 0.0) (ms <|> m)
    fmap Angle (fromDMS d' m' s')

-- | Parses minutes and seconds.
ms :: ReadP (Int, Double)
ms = do
    m' <- integer
    _ <- symbol
    s' <- number
    _ <- symbol
    return (m', s')

-- | Parses minutes.
m :: ReadP (Int, Double)
m = do
    m' <- integer
    _ <- symbol
    return (m', 0.0)

-- | Parses decimal degrees.
decimal :: ReadP Angle
decimal = do
    d <- double
    _ <- symbol
    return (Angle d)

-- | Parses all non alphanumeric characters expect '.'.
symbol :: ReadP String
symbol = munch1 (\c -> not (isAlphaNum c) && c /= '.')
