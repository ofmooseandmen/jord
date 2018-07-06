module Data.Geo.Jord.Expr.Token
    ( Token(..)
    , tokenise
    ) where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Text.ParserCombinators.ReadP

data Token
    = Paren Char
    | Func String
    | Str String
    deriving (Eq, Show)

-- | TODO: doc
-- TODO: wrap s between '()' if needed
-- s = "destination (readGeoPos P1) (finalBearing P2 P3) (distance (antipode P4) (readGeoPos P5))" `shouldBe`
-- ts = tokenise s
-- ts => [ Func "destination"
--       , Paren '('
--       , Func "readGeoPos"
--       , Str "P1"
--       , Paren ')'
--       , Paren '('
--       , Func "finalBearing"
--       , Str "P2"
--       , Str "P3"
--       , Paren ')'
--       , Paren '('
--       , Func "distance"
--       , Paren '('
--       , Func "antipode"
--       , Str "P4"
--       , Paren ')'
--       , Paren '('
--       , Func "readGeoPos"
--       , Str "P5"
--       , Paren ')'
--       , Paren ')'
--       ]
tokenise :: String -> Either String [Token]
tokenise s =
  case last (readP_to_S tokens s) of
    (e, "") -> Right e
    r -> Left ("Invalid text: " ++ snd r)

tokens :: ReadP [Token]
tokens = many1 token

token :: ReadP Token
token = (<++) ((<++) paren func) str

paren :: ReadP Token
paren = do
    skipSpaces
    c <- char '(' <|> char ')'
    skipSpaces
    return (Paren c)

func :: ReadP Token
func = do
  n <- choice (map string functions)
  _ <- char ' '
  return (Func n)

str :: ReadP Token
str = do
  v <- munch1 isAlphaNum
  skipSpaces
  return (Str v)

functions :: [String]
functions = ["antipode", "destination", "distance", "finalBearing", "initialBearing", "readGeoPos"]
