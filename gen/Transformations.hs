module Transformations
    ( Transformation(..)
    , parser
    , generator
    ) where

import Data.List (intersperse)
import Data.Maybe (isJust)
import Text.ParserCombinators.ReadP (ReadP, skipSpaces, string)

import qualified Generator as G
import qualified Parsers as P

data Params =
    Params [Double] Double [Double]

data Transformation =
    Transformation
        { comment :: [String]
        , from :: String
        , to :: String
        , epoch :: Maybe Double
        , params :: Params
        , rates :: Params
        }

parser :: ReadP Transformation
parser = do
    c <- P.comment
    f <- P.name
    _ <- string " -> "
    t <- P.name
    P.eol
    me <- P.epoch
    case me of
        Nothing -> do
            ps <- params' "params:"
            P.eol
            return (Transformation c f t Nothing ps noRates)
        (Just _) -> do
            P.eol
            ps <- params' "params:"
            rs <- params' "rates:"
            P.eol
            return (Transformation c f t me ps rs)

noRates :: Params
noRates = Params [] 0.0 []

params' :: String -> ReadP Params
params' n = do
    skipSpaces
    _ <- string n
    skipSpaces
    tx <- P.number
    skipSpaces
    ty <- P.number
    skipSpaces
    tz <- P.number
    skipSpaces
    s <- P.number
    skipSpaces
    rx <- P.number
    skipSpaces
    ry <- P.number
    skipSpaces
    rz <- P.number
    return (Params [tx, ty, tz] s [rx, ry, rz])

generator :: G.Generator Transformation
generator =
    G.Generator ["Data.Geo.Jord.Model", "Data.Geo.Jord.Transformation"] transformationToString

transformationToString :: Transformation -> String
transformationToString t
    | isJust (epoch t) = dynamicTx t
    | otherwise = staticTx t

dynamicTx :: Transformation -> String
dynamicTx t =
    G.commentToString (comment t) ++
    func t ++
    " :: DynamicTx\n" ++
    func t ++
    " =\n    dynamicTx\n" ++
    "        " ++
    (idToString (from t)) ++
    "\n        " ++
    (idToString (to t)) ++
    "\n        " ++
    "(TxParams15" ++
    "\n             " ++
    (epochToString (epoch t)) ++
    "\n             " ++
    tx7ToString (params t) ++ "\n             " ++ ratesToString (rates t) ++ ")"

staticTx :: Transformation -> String
staticTx t =
    G.commentToString (comment t) ++
    func t ++
    " :: StaticTx\n" ++
    func t ++
    " =\n    staticTx\n" ++
    "        " ++
    (idToString (from t)) ++
    "\n        " ++ (idToString (from t)) ++ "\n        " ++ (tx7ToString (params t))

idToString :: String -> String
idToString s = "(ModelId \"" ++ s ++ "\")"

tx7ToString :: Params -> String
tx7ToString (Params t s r) =
    "(txParams7 " ++ dsToString t ++ " " ++ dToString s ++ " " ++ dsToString r ++ ")"

ratesToString :: Params -> String
ratesToString (Params t s r) =
    "(txRates " ++ dsToString t ++ " " ++ dToString s ++ " " ++ dsToString r ++ ")"

dsToString :: [Double] -> String
dsToString ds = "(" ++ (concat (intersperse ", " (map show ds))) ++ ")"

dToString :: Double -> String
dToString d
    | d < 0 = "(" ++ show d ++ ")"
    | otherwise = show d

func :: Transformation -> String
func t = "from_" ++ from t ++ "_to_" ++ to t

epochToString :: Maybe Double -> String
epochToString Nothing = error "no epoch"
epochToString (Just yd) = "(Epoch " ++ show yd ++ ")"
