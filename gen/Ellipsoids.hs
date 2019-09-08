module Ellipsoids
    ( Ellipsoid(..)
    , parser
    , generator
    ) where

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP (ReadP, char, skipSpaces, string)

import Generator
import qualified Parsers as P

data Ellipsoid =
    Ellipsoid
        { name :: String
        , comment :: String
        , params :: Either (Double, Double) Double
        }

parser :: ReadP Ellipsoid
parser = do
    c <- P.comment
    P.eol
    n <- P.name
    P.eol
    ps <- params'
    P.eol
    return (Ellipsoid n c ps)

params' :: ReadP (Either (Double, Double) Double)
params' = fmap Left eparams <|> fmap Right sparams

eparams :: ReadP (Double, Double)
eparams = do
    skipSpaces
    _ <- string "a:"
    skipSpaces
    a <- radius
    P.eol
    skipSpaces
    _ <- string "1/f:"
    skipSpaces
    invf <- P.number
    return (a, invf)

sparams :: ReadP Double
sparams = do
    skipSpaces
    _ <- string "r:"
    skipSpaces
    radius

radius :: ReadP Double
radius = do
    d <- P.number
    _ <- char 'm'
    return d

generator :: Generator Ellipsoid
generator = Generator ["Data.Geo.Jord.Ellipsoid", "Data.Geo.Jord.Length"] ellipsoidToString

ellipsoidToString :: Ellipsoid -> String
ellipsoidToString e =
    "-- | " ++
    comment e ++
    ".\n" ++ "e" ++ name e ++ " :: Ellipsoid" ++ "\n" ++ "e" ++ name e ++ " = " ++ value
  where
    value =
        case params e of
            Left (a, invf) -> "ellispoid (metres " ++ show a ++ ") " ++ show invf
            Right r -> "sphere (metres " ++ show r ++ ")"