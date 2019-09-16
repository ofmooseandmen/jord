module Models
    ( Model(..)
    , parser
    , generator
    ) where

import Control.Applicative ((<|>))
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP, choice, skipSpaces, string)

import qualified Generator as G
import qualified Parsers as P

data Model =
    Model
        { mtype :: ModelType
        , mid :: String
        , comment :: [String]
        , surface :: String
        , longitudeRange :: String
        , epoch :: Maybe Double
        }

data ModelType
    = Spherical
    | Ellipsoidal
    deriving (Eq)

parser :: ReadP Model
parser = do
    c <- P.comment
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

maybeEpoch :: ModelType -> ReadP (Maybe Double)
maybeEpoch Spherical = return Nothing
maybeEpoch _ = P.epoch

generator :: String -> G.Generator Model
generator ellipsoids =
    G.Generator [ellipsoids, "Data.Geo.Jord.Ellipsoid", "Data.Geo.Jord.Model"] genModel (const "")

genModel :: Model -> String
genModel m = unlines' ([d, model, eq, show'] ++ instanceType m)
  where
    d = G.documentation (comment m) ++ "data " ++ mid m ++ " = " ++ "\n" ++ "    " ++ mid m
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

instanceEllipsoidal :: String -> Maybe Double -> [String]
instanceEllipsoidal n Nothing = ["instance Ellipsoidal " ++ n]
instanceEllipsoidal n (Just yd) =
    [ "instance Ellipsoidal " ++ n
    , "instance EllipsoidalT0 " ++ n ++ " where\n" ++ "    epoch _ = Epoch " ++ show yd
    ]

unlines' :: [String] -> String
unlines' = intercalate "\n\n"
