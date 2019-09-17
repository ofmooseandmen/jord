-- |
-- Module:      Data.Geo.Jord.Length
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with (signed) lengths in metres, kilometres, nautical miles or feet.
--
module Data.Geo.Jord.Length
    (
    -- * The 'Length' type
      Length
    -- * Smart constructors
    , feet
    , kilometres
    , metres
    , nauticalMiles
    -- * Read
    , lengthP
    , readLength
    -- * Conversions
    , toFeet
    , toKilometres
    , toMetres
    , toMillimetres
    , toNauticalMiles
    ) where

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP (ReadP, pfail, readP_to_S, skipSpaces, string)
import Text.Read (readMaybe)

import Data.Geo.Jord.Parser
import Data.Geo.Jord.Quantity

-- | A length with a resolution of 1 micrometre.
newtype Length =
    Length
        { micrometre :: Int
        }
    deriving (Eq)

-- | See 'lengthP'.
instance Read Length where
    readsPrec _ = readP_to_S lengthP

-- | Length is shown in metres when absolute value is <= 10 km and in kilometres otherwise.
instance Show Length where
    show l
        | abs' l <= (kilometres 10) = show (toMetres l) ++ "m"
        | otherwise = show (toKilometres l) ++ "km"

instance Ord Length where
    (<=) (Length l1) (Length l2) = l1 <= l2

-- | Add/Subtract 'Length's.
instance Quantity Length where
    add a b = Length (micrometre a + micrometre b)
    sub a b = Length (micrometre a - micrometre b)
    zero = Length 0

-- | 'Length' from given amount of feet.
feet :: Double -> Length
feet ft = Length (round (ft * 0.3048 * m2um))

-- | 'Length' from given amount of kilometres.
kilometres :: Double -> Length
kilometres km = Length (round (km * 1000.0 * m2um))

-- | 'Length' from given amount of metres.
metres :: Double -> Length
metres m = Length (round (m * m2um))

-- | 'Length' from given amount of nautical miles.
nauticalMiles :: Double -> Length
nauticalMiles nm = Length (round (nm * 1852.0 * m2um))

-- | Reads an a 'Length' from the given string using 'lengthP'.
readLength :: String -> Maybe Length
readLength s = readMaybe s :: (Maybe Length)

-- | @toFeet l@ converts @l@ to feet.
toFeet :: Length -> Double
toFeet (Length l) = fromIntegral l / (0.3048 * m2um)

-- | @toKilometres l@ converts @l@ to kilometres.
toKilometres :: Length -> Double
toKilometres (Length l) = fromIntegral l / (1000.0 * m2um)

-- | @toMetres l@ converts @l@ to metres.
toMetres :: Length -> Double
toMetres (Length l) = fromIntegral l / m2um

-- | @toMillimetres l@ converts @l@ to millimetres.
toMillimetres :: Length -> Double
toMillimetres (Length l) = fromIntegral l / 1000.0

-- | @toNauticalMiles l@ converts @l@ to nautical miles.
toNauticalMiles :: Length -> Double
toNauticalMiles (Length l) = fromIntegral l / (1852.0 * m2um)

-- | Parses and returns a 'Length' formatted as (-)float[m|km|nm|ft].
-- e.g. 3000m, 2.5km, -154nm or 10000ft.
--
lengthP :: ReadP Length
lengthP = do
    v <- number
    skipSpaces
    u <- string "m" <|> string "km" <|> string "nm" <|> string "ft"
    case u of
        "m" -> return (metres v)
        "km" -> return (kilometres v)
        "nm" -> return (nauticalMiles v)
        "ft" -> return (feet v)
        _ -> pfail

-- | metre to micrometre.
m2um :: Double
m2um = 1000.0 * 1000.0

abs' :: Length -> Length
abs' (Length um) = Length (abs um)
