module Transformations
    ( Transformation(..)
    , parser
    , generator
    ) where

import Data.List (intercalate, partition)
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
generator = G.Generator ["Data.Geo.Jord.Model", "Data.Geo.Jord.Tx"] genTx genAll

genTx :: Transformation -> String
genTx t
    | isJust (epoch t) = timeDependentTx t
    | otherwise = fixedTx t

timeDependentTx :: Transformation -> String
timeDependentTx t =
    G.documentation (comment t) ++
    func t ++
    " :: Tx Params15\n" ++
    func t ++
    " =\n    Tx " ++
    idToString (from t) ++
    "\n        " ++
    idToString (to t) ++
    "\n        " ++
    "(Params15" ++
    "\n             " ++
    epochToString (epoch t) ++
    "\n             " ++
    tx7ToString (params t) ++ "\n             " ++ ratesToString (rates t) ++ ")"

fixedTx :: Transformation -> String
fixedTx t =
    G.documentation (comment t) ++
    func t ++
    " :: Tx Params7\n" ++
    func t ++
    " =\n    Tx " ++
    idToString (from t) ++
    "\n        " ++ idToString (to t) ++ "\n        " ++ tx7ToString (params t)

idToString :: String -> String
idToString s = "(ModelId \"" ++ s ++ "\")"

tx7ToString :: Params -> String
tx7ToString (Params t s r) =
    "(params7 " ++ dsToString t ++ " " ++ dToString s ++ " " ++ dsToString r ++ ")"

ratesToString :: Params -> String
ratesToString (Params t s r) =
    "(rates " ++ dsToString t ++ " " ++ dToString s ++ " " ++ dsToString r ++ ")"

dsToString :: [Double] -> String
dsToString ds = "(" ++ intercalate ", " (map show ds) ++ ")"

dToString :: Double -> String
dToString d
    | d < 0 = "(" ++ show d ++ ")"
    | otherwise = show d

func :: Transformation -> String
func t = "from_" ++ from t ++ "_to_" ++ to t

epochToString :: Maybe Double -> String
epochToString Nothing = error "no epoch"
epochToString (Just yd) = "(Epoch " ++ show yd ++ ")"

genAll :: [Transformation] -> String
genAll ts = genFixedTxs s ++ "\n" ++ genTimeDependentTxs d
  where
    (d, s) = split ts

genFixedTxs :: [Transformation] -> String
genFixedTxs ts =
    "-- | Graph of all transformations between fixed models.\n\
   \fixed :: Graph Params7\n\
   \fixed =\n\
   \    graph\n\
   \        [ " ++
    funcs ts ++ "\n        ]\n"

genTimeDependentTxs :: [Transformation] -> String
genTimeDependentTxs ts =
    "-- | Graph of all transformations between time-dependent models.\n\
   \timeDependent :: Graph Params15\n\
   \timeDependent =\n\
   \    graph\n\
   \        [ " ++
    funcs ts ++ "\n        ]\n"

funcs :: [Transformation] -> String
funcs ts = intercalate "\n        , " (map func ts)

split :: [Transformation] -> ([Transformation], [Transformation])
split = partition (isJust . epoch)
