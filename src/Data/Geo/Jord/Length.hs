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
    ( Length(millimetres)
    , isZero
    , kilometres
    , metres
    , nauticalMiles
    , readLength
    , readLengthE
    , readLengthF
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

-- | A length - the value is internally stored in millimetres.
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

-- | Is given 'Length' == 0?
isZero :: Length -> Bool
isZero (Length mm) = mm == 0

-- | Returns a 'Length' representing the given amount of nautical miles.
nauticalMiles :: Double -> Length
nauticalMiles nm = metres (nm * 1852.0)

-- | Returns a 'Length' representing the given amount of metres.
metres :: Double -> Length
metres m = Length (round (m * 1000.0))

-- | Returns a 'Length' representing the given amount of kilometres.
kilometres :: Double -> Length
kilometres km = metres (km * 1000.0)

-- | Obtains a 'Length' from the given string formatted as (-)float[m|km|nm] - e.g. 3000m, 2.5km or -154nm.
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

-- | Returns amount of kilometres that the given 'Length' represents.
toKilometres :: Length -> Double
toKilometres l = toMetres l / 1000.0

-- | Returns amount of metres that the given 'Length' represents.
toMetres :: Length -> Double
toMetres (Length mm) = fromIntegral mm / 1000.0

-- | Returns amount of nautical miles that the given 'Length' represents.
toNauticalMiles :: Length -> Double
toNauticalMiles l = toMetres l / 1852.0

-- | Parses and returns a 'Length'.
length :: ReadP Length
length = do
    v <- number
    skipSpaces
    u <- string "m" <|> string "km" <|> string "Nm"
    case u of
        "m" -> return (metres v)
        "km" -> return (kilometres v)
        "Nm" -> return (nauticalMiles v)
        _ -> pfail
