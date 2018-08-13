-- |
-- Module:      Data.Geo.Jord.Length
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with (signed) lengths in metres, kilometres or nautical miles.
--
module Data.Geo.Jord.Length
    (
    -- * The 'Length' type
      Length(millimetres)
    -- * Smart constructors
    , feet
    , kilometres
    , metres
    , nauticalMiles
    -- * Read
    , readLength
    , readLengthE
    , readLengthF
    -- * Conversions
    , toFeet
    , toKilometres
    , toMetres
    , toNauticalMiles
    ) where

import Control.Applicative
import Control.Monad.Fail
import Data.Geo.Jord.Parse
import Data.Geo.Jord.Quantity
import Prelude hiding (fail, length)
import Text.ParserCombinators.ReadP
import Text.Read hiding (pfail)

-- | A length with a resolution of 1 millimetre.
newtype Length = Length
    { millimetres :: Int
    } deriving (Eq)

-- | See 'readLength'.
instance Read Length where
    readsPrec _ = readP_to_S length

-- | Length is shown in metres when <= 10,000 m and in kilometres otherwise.
instance Show Length where
    show l
        | m <= 10000.0 = show m ++ "m"
        | otherwise = show (m / 1000.0) ++ "km"
      where
        m = toMetres l

-- | Add/Subtract Length.
instance Quantity Length where
    add a b = Length (millimetres a + millimetres b)
    sub a b = Length (millimetres a - millimetres b)
    zero = Length 0

-- | 'Length' from given amount of feet.
feet :: Double -> Length
feet ft = metres (ft * 0.3048)

-- | 'Length' from given amount of kilometres.
kilometres :: Double -> Length
kilometres km = metres (km * 1000.0)

-- | 'Length' from given amount of metres.
metres :: Double -> Length
metres m = Length (round (m * 1000.0))

-- | 'Length' from given amount of nautical miles.
nauticalMiles :: Double -> Length
nauticalMiles nm = metres (nm * 1852.0)

-- | Obtains a 'Length' from the given string formatted as (-)float[m|km|nm|ft] - e.g. 3000m, 2.5km, -154nm or 10000ft.
--
-- This simply calls @read s :: Length@ so 'error' should be handled at the call site.
--
readLength :: String -> Length
readLength s = read s :: Length

-- | Same as 'readLength' but returns a 'Either'.
readLengthE :: String -> Either String Length
readLengthE s =
    case readMaybe s of
        Nothing -> Left ("couldn't read length " ++ s)
        Just l -> Right l

-- | Same as 'readLength' but returns a 'MonadFail'.
readLengthF :: (MonadFail m) => String -> m Length
readLengthF s =
    let p = readEither s
     in case p of
            Left e -> fail e
            Right l -> return l

-- | @toFeet l@ converts @l@ to feet.
toFeet :: Length -> Double
toFeet l = toMetres l / 0.3048

-- | @toKilometres l@ converts @l@ to kilometres.
toKilometres :: Length -> Double
toKilometres l = toMetres l / 1000.0

-- | @toMetres l@ converts @l@ to metres.
toMetres :: Length -> Double
toMetres (Length mm) = fromIntegral mm / 1000.0

-- | @toNauticalMiles l@ converts @l@ to nautical miles.
toNauticalMiles :: Length -> Double
toNauticalMiles l = toMetres l / 1852.0

-- | Parses and returns a 'Length'.
length :: ReadP Length
length = do
    v <- number
    skipSpaces
    u <- string "m" <|> string "km" <|> string "Nm" <|> string "ft"
    case u of
        "m" -> return (metres v)
        "km" -> return (kilometres v)
        "Nm" -> return (nauticalMiles v)
        "ft" -> return (feet v)
        _ -> pfail
