module Parsers
    ( comment
    , name
    , number
    , integer
    , eol
    , module'
    , epoch
    ) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (stripPrefix)
import Data.Maybe (isJust)
import Text.ParserCombinators.ReadP
    ( ReadP
    , char
    , look
    , many
    , many1
    , munch1
    , option
    , satisfy
    , skipSpaces
    , string
    )

comment :: ReadP [String]
comment = many commentL

commentL :: ReadP String
commentL = do
    _ <- char '#'
    c <- many (satisfy (\c -> c /= '\n' && c /= '\r'))
    eol
    return c

name :: ReadP String
name = many1 (satisfy (\c -> c == '_' || isAlphaNum c))

number :: ReadP Double
number = double <|> fmap fromIntegral integer

double :: ReadP Double
double = do
    s <- option 1.0 (fmap (\_ -> -1.0) (char '-'))
    i <- natural
    f <- char '.' >> munch1 isDigit
    return (s * (read (show i ++ "." ++ f) :: Double))

integer :: ReadP Int
integer = do
    s <- option 1 (fmap (\_ -> -1) (char '-'))
    p <- natural
    return (s * p)

natural :: ReadP Int
natural = fmap read (munch1 isDigit)

eol :: ReadP ()
eol = do
    _ <- many1 (char '\n' <|> (char '\r' >> char '\n'))
    return ()

module' :: ReadP String
module' = do
    _ <- string "module "
    many1 (satisfy (\c -> c == '.' || isAlphaNum c))

epoch :: ReadP (Maybe Double)
epoch = do
    n <- look
    if hasEpoch n
        then fmap Just epoch'
        else return Nothing

hasEpoch :: String -> Bool
hasEpoch s = isJust (stripPrefix "epoch" (dropWhile (not . isAlpha) s))

epoch' :: ReadP Double
epoch' = do
    skipSpaces
    _ <- string "epoch: "
    double
