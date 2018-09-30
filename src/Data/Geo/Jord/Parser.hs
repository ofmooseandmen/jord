-- |
-- Module:      Data.Geo.Jord.Parser
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- internal 'ReadP' parsers used by "Jord".
--
module Data.Geo.Jord.Parser
    ( digits
    , double
    , integer
    , natural
    , number
    ) where

import Control.Applicative
import Data.Char
import Text.ParserCombinators.ReadP

-- | Parses the given number of digits and returns the read 'Int'.
digits :: Int -> ReadP Int
digits n = fmap read (count n digit)

-- | Parses optionally a @-@ followed by a 'positive'.'positive' and returns the read 'Double'.
double :: ReadP Double
double = do
    s <- option 1.0 (fmap (\_ -> -1.0) (char '-'))
    i <- natural
    f <- char '.' >> natural
    return (s * (read (show i ++ "." ++ show f) :: Double))

-- | Parses optionally a @-@ followed by a 'positive' and returns the read 'Int'.
integer :: ReadP Int
integer = do
    s <- option 1 (fmap (\_ -> -1) (char '-'))
    p <- natural
    return (s * p)

-- | Parses 1 or more 'digit's and returns the read 'Int'.
natural :: ReadP Int
natural = fmap read (munch1 isDigit)

-- | Parses an 'integer' or 'double' and returns the read 'Double'.
number :: ReadP Double
number = double <|> fmap fromIntegral integer

-- | Parses and returns a digit.
digit :: ReadP Char
digit = satisfy isDigit
