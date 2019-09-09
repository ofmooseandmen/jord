module Parsers
    ( comment
    , name
    , number
    , integer
    , eol
    , module'
    ) where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit)
import Text.ParserCombinators.ReadP
    ( ReadP
    , char
    , many1
    , munch1
    , option
    , satisfy
    , skipSpaces
    , string
    )

comment :: ReadP String
comment = do
    _ <- char '#'
    skipSpaces
    c <- many1 (satisfy (/= '#'))
    _ <- char '#'
    case last c of
        ' ' -> return (init c)
        _ -> return c

name :: ReadP String
name = many1 (satisfy (\c -> c == '_' || isAlphaNum c))

number :: ReadP Double
number = double <|> fmap fromIntegral integer

double :: ReadP Double
double = do
    s <- option 1.0 (fmap (\_ -> -1.0) (char '-'))
    i <- natural
    f <- char '.' >> natural
    return (s * (read (show i ++ "." ++ show f) :: Double))

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
    _ <- string "module: "
    many1 (satisfy (\c -> c == '.' || isAlphaNum c))
