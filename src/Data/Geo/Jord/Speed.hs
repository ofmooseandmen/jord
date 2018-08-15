-- |
-- Module:      Data.Geo.Jord.Speed
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with speed in metres per second, kilometres per hour, miles per hour or knots.
--
module Data.Geo.Jord.Speed
    -- * The 'Speed' type
    ( Speed(metresPerHour)
    -- * Smart constructors
    , metresPerSecond
    , kilometresPerHour
    , milesPerHour
    , knots
    -- * Read
    , readSpeed
    , readSpeedE
    , readSpeedF
    -- * Conversions
    , toMetresPerSecond
    , toKilometresPerHour
    , toMilesPerHour
    , toKnots
    ) where

import Control.Applicative
import Control.Monad.Fail
import Data.Geo.Jord.Parse
import Data.Geo.Jord.Quantity
import Prelude hiding (fail)
import Text.ParserCombinators.ReadP
import Text.Read hiding (pfail)

-- | A speed with a resolution of 1 metre per hour.
newtype Speed = Speed
    { metresPerHour :: Int
    } deriving (Eq)

-- | See 'readSpeed'.
instance Read Speed where
    readsPrec _ = readP_to_S speed

-- | Speed is shown in kilometres per hour.
instance Show Speed where
    show s = show (toKilometresPerHour s) ++ "km/h"

-- | Add/Subtract Speed.
instance Quantity Speed where
    add a b = Speed (metresPerHour a + metresPerHour b)
    sub a b = Speed (metresPerHour a - metresPerHour b)
    zero = Speed 0

-- | 'Speed' from given amount of metres per second.
metresPerSecond :: Double -> Speed
metresPerSecond mps = kilometresPerHour (mps * 3.6)

-- | 'Speed' from given amount of kilometres per hour.
kilometresPerHour :: Double -> Speed
kilometresPerHour kph = Speed (round (kph * 1000))

-- | 'Speed' from given amount of miles per hour.
milesPerHour :: Double -> Speed
milesPerHour mph = kilometresPerHour (mph * 1.609344)

-- | 'Speed' from given amount of knots.
knots :: Double -> Speed
knots kt = kilometresPerHour (kt * 1.852)

-- | Obtains a 'Speed' from the given string formatted as (-)float[m/s|km/h|mph|kt] - e.g. 300m/s, 250km/h, -154mph or 400kt.
--
-- This simply calls @read s :: Speed@ so 'error' should be handled at the call site.
--
readSpeed :: String -> Speed
readSpeed s = read s :: Speed

-- | Same as 'readSpeed' but returns a 'Either'.
readSpeedE :: String -> Either String Speed
readSpeedE s =
    case readMaybe s of
        Nothing -> Left ("couldn't read speed " ++ s)
        Just l -> Right l

-- | Same as 'readSpeed' but returns a 'MonadFail'.
readSpeedF :: (MonadFail m) => String -> m Speed
readSpeedF s =
    let p = readEither s
     in case p of
            Left e -> fail e
            Right l -> return l

-- | @toMetresPerSecond s@ converts @s@ to metres per second.
toMetresPerSecond :: Speed -> Double
toMetresPerSecond s = toKilometresPerHour s / 3.6

-- | @toKilometresPerHour s@ converts @s@ to kilometres per hour.
toKilometresPerHour :: Speed -> Double
toKilometresPerHour (Speed s) = fromIntegral s / 1000.0

-- | @toMilesPerHour s@ converts @s@ to miles per hour.
toMilesPerHour :: Speed -> Double
toMilesPerHour s = toKilometresPerHour s / 1.609344

-- | @toKnots s@ converts @s@ to knots.
toKnots :: Speed -> Double
toKnots s = toKilometresPerHour s / 1.852

-- | Parses and returns a 'Speed'.
speed :: ReadP Speed
speed = do
    v <- number
    skipSpaces
    u <- string "m/s" <|> string "km/h" <|> string "mph" <|> string "kt"
    case u of
        "m/s" -> return (metresPerSecond v)
        "km/h" -> return (kilometresPerHour v)
        "mph" -> return (milesPerHour v)
        "kt" -> return (knots v)
        _ -> pfail
