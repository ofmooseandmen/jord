-- |
-- Module:      Data.Geo.Jord.Length
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with lengths in metres, kilometres or nautical miles.
--
module Data.Geo.Jord.Length
    ( Length(metres)
    , ofMetres
    , ofKilometres
    , ofNauticalMiles
    , readLength
    , readLengthE
    , readLengthF
    , kilometres
    , nauticalMiles
    ) where

import Control.Applicative
import Control.Monad.Fail
import Data.Geo.Jord.Parse
import Data.Geo.Jord.Quantity
import Prelude hiding (fail, length)
import Text.ParserCombinators.ReadP
import Text.Read hiding (pfail)

-- | A length - the value is internally stored in metres.
newtype Length = Length
    { metres :: Double
    } deriving (Eq)

-- | See 'readLength'.
instance Read Length where
    readsPrec _ = readP_to_S length

-- | Length is shown in metres when <= 10,000 m and in kilometres otherwise.
instance Show Length where
    show (Length v)
        | v <= 10000.0 = show v ++ "m"
        | otherwise = show (v / 1000.0) ++ "km"

-- | Add/Subtract Length.
instance Quantity Length where
    add a b = Length (metres a + metres b)
    sub a b = Length (metres a - metres b)

-- | Returns a 'Length' representing the given amount of metres.
ofMetres :: Double -> Length
ofMetres = Length

-- | Returns a 'Length' representing the given amount of kilometres.
ofKilometres :: Double -> Length
ofKilometres km = Length (km * 1000.0)

-- | Returns a 'Length' representing the given amount of nautical miles.
ofNauticalMiles :: Double -> Length
ofNauticalMiles nm = Length (nm * 1852.0)

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
kilometres :: Length -> Double
kilometres l = metres l / 1000.0

-- | Returns amount of nautical miles that the given 'Length' represents.
nauticalMiles :: Length -> Double
nauticalMiles l = metres l / 1852.0

-- | Parses and returns a 'Length'.
length :: ReadP Length
length = do
    v <- number
    skipSpaces
    u <- string "m" <|> string "km" <|> string "Nm"
    case u of
        "m" -> return (Length v)
        "km" -> return (ofKilometres v)
        "Nm" -> return (ofNauticalMiles v)
        _ -> pfail
