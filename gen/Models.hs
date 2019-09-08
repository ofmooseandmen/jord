module Models
    ( Model(..)
    , parser
    , generator
    ) where

import Control.Applicative ((<|>))
import Data.List (intersperse)
import Text.ParserCombinators.ReadP (ReadP, char, choice, skipSpaces, string)

import Generator
import qualified Parsers as P

data Model =
    Model
        { mtype :: ModelType
        , name :: String
        , comment :: String
        , surface :: String
        , longitudeRange :: String
        , epoch :: (Int, Int)
        }

data ModelType
    = Spherical
    | EllipsoidalStatic
    | EllipsoidalDynamic
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
    ep <- epoch' t
    P.eol
    return (Model t n c s lr ep)

type' :: ReadP ModelType
type' = do
    s <- string "spherical" <|> string "ellipsoidal static" <|> string "ellipsoidal dynamic"
    case s of
        "spherical" -> return Spherical
        "ellipsoidal static" -> return EllipsoidalStatic
        _ -> return EllipsoidalDynamic

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

epoch' :: ModelType -> ReadP (Int, Int)
epoch' t =
    case t of
        EllipsoidalDynamic -> epoch''
        _ -> return (0, 0)

epoch'' :: ReadP (Int, Int)
epoch'' = do
    skipSpaces
    _ <- string "epoch: "
    y <- P.integer
    _ <- char '.'
    d <- P.integer
    return (y, d)

generator :: String -> Generator Model
generator ellipsoids =
    Generator [ellipsoids, "Data.Geo.Jord.Ellipsoid", "Data.Geo.Jord.Model"] modelToString

modelToString :: Model -> String
modelToString m = unlines' ([d, model, eq, show'] ++ instanceType m)
  where
    d = "-- | " ++ comment m ++ ".\ndata " ++ name m ++ " = " ++ "\n" ++ "    " ++ name m
    model = instanceModel m
    eq = instanceEq m
    show' = instanceShow m

instanceModel :: Model -> String
instanceModel m =
    "instance Model " ++
    name m ++
    " where\n" ++
    "    surface _ = " ++
    s ++
    "\n" ++
    "    longitudeRange _ = " ++
    longitudeRange m ++ "\n" ++ "    name _ = ModelName \"" ++ name m ++ "\""
  where
    s =
        if mtype m == Spherical
            then "toSphere e" ++ surface m
            else "e" ++ surface m

instanceEq :: Model -> String
instanceEq m = "instance Eq " ++ name m ++ " where\n    _ == _ = True"

instanceShow :: Model -> String
instanceShow m = "instance Show " ++ name m ++ " where\n    show m = show (name m)"

instanceType :: Model -> [String]
instanceType m
    | mtype m == Spherical = instanceSpherical n
    | mtype m == EllipsoidalStatic = instanceStatic n
    | mtype m == EllipsoidalDynamic = instanceDynamic n (epoch m)
    | otherwise = error "unsupported type"
  where
    n = name m

instanceSpherical :: String -> [String]
instanceSpherical n = ["instance Spherical " ++ n]

instanceStatic :: String -> [String]
instanceStatic n = ["instance Ellipsoidal " ++ n, "instance StaticSurface " ++ n]

instanceDynamic :: String -> (Int, Int) -> [String]
instanceDynamic n (y, d) =
    [ "instance Ellipsoidal " ++ n
    , "instance DynamicSurface " ++
      n ++ " where\n" ++ "    epoch _ = Epoch " ++ show y ++ " " ++ show d
    ]

unlines' :: [String] -> String
unlines' s = foldl (++) "" (intersperse "\n\n" s)