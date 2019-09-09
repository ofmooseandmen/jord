module Models
    ( Model(..)
    , parser
    , generator
    ) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha)
import Data.List (intercalate, stripPrefix)
import Data.Maybe (isJust)
import Text.ParserCombinators.ReadP (ReadP, char, choice, look, skipSpaces, string)

import qualified Generator as G
import qualified Parsers as P

data Model =
    Model
        { mtype :: ModelType
        , mid :: String
        , comment :: String
        , surface :: String
        , longitudeRange :: String
        , epoch :: Maybe (Int, Int)
        }

data ModelType
    = Spherical
    | Ellipsoidal
    deriving (Eq)

parser :: ReadP Model
parser = do
    c <- P.comment
    P.eol
    t <- type'
    skipSpaces
    n <- P.name
    P.eol
    s <- surface'
    P.eol
    lr <- longitudeRange'
    ep <- maybeEpoch t
    P.eol
    return (Model t n c s lr ep)

type' :: ReadP ModelType
type' = do
    s <- string "spherical " <|> string "ellipsoidal "
    case s of
        "spherical " -> return Spherical
        "ellipsoidal " -> return Ellipsoidal
        _ -> error "unsupported model type"

surface' :: ReadP String
surface' = do
    skipSpaces
    _ <- string "surface: "
    P.name

longitudeRange' :: ReadP String
longitudeRange' = do
    skipSpaces
    _ <- string "longitudeRange: "
    choice [string "L180", string "L360"]

maybeEpoch :: ModelType -> ReadP (Maybe (Int, Int))
maybeEpoch Spherical = return Nothing
maybeEpoch _ = do
    n <- look
    if hasEpoch n
        then fmap Just epoch'
        else return Nothing

hasEpoch :: String -> Bool
hasEpoch s = isJust (stripPrefix "epoch" (dropWhile (not . isAlpha) s))

epoch' :: ReadP (Int, Int)
epoch' = do
    skipSpaces
    _ <- string "epoch: "
    y <- P.integer
    _ <- char '.'
    d <- P.integer
    return (y, d)

generator :: String -> G.Generator Model
generator ellipsoids =
    G.Generator [ellipsoids, "Data.Geo.Jord.Ellipsoid", "Data.Geo.Jord.Model"] modelToString

modelToString :: Model -> String
modelToString m = unlines' ([d, model, eq, show'] ++ instanceType m)
  where
    d = "-- | " ++ comment m ++ ".\ndata " ++ mid m ++ " = " ++ "\n" ++ "    " ++ mid m
    model = instanceModel m
    eq = instanceEq m
    show' = instanceShow m

instanceModel :: Model -> String
instanceModel m =
    "instance Model " ++
    mid m ++
    " where\n" ++
    "    modelId _ = ModelId \"" ++
    mid m ++
    "\"\n" ++ "    surface _ = " ++ s ++ "\n" ++ "    longitudeRange _ = " ++ longitudeRange m
  where
    s =
        if mtype m == Spherical
            then "toSphere e" ++ surface m
            else "e" ++ surface m

instanceEq :: Model -> String
instanceEq m = "instance Eq " ++ mid m ++ " where\n    _ == _ = True"

instanceShow :: Model -> String
instanceShow m = "instance Show " ++ mid m ++ " where\n    show m = show (modelId m)"

instanceType :: Model -> [String]
instanceType m
    | mtype m == Spherical = instanceSpherical n
    | mtype m == Ellipsoidal = instanceEllipsoidal n (epoch m)
    | otherwise = error "unsupported type"
  where
    n = mid m

instanceSpherical :: String -> [String]
instanceSpherical n = ["instance Spherical " ++ n]

instanceEllipsoidal :: String -> Maybe (Int, Int) -> [String]
instanceEllipsoidal n Nothing = ["instance Ellipsoidal " ++ n]
instanceEllipsoidal n (Just (y, d)) =
    [ "instance Ellipsoidal " ++ n
    , "instance EllipsoidalT0 " ++
      n ++ " where\n" ++ "    epoch _ = Epoch " ++ show y ++ " " ++ show d
    ]

unlines' :: [String] -> String
unlines' = intercalate "\n\n"