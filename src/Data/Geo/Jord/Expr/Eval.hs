module Data.Geo.Jord.Expr.Eval
    ( eval
    ) where

import Data.Geo.Jord

data Expr
    = Param String
    | Antipode Expr
    | Distance Expr
               Expr
    | FinalBearing Expr
                   Expr
    | InitialBearing Expr
                     Expr
    | ReadGeo String
    deriving (Eq, Show)

data Result
    = Error String
    | OkAng Angle
    | OkLen Length
    | OkPos GeoPos
    deriving (Show)

eval :: Expr -> Result
eval (Param p) =
    case r of
        [(OkAng a), _, _] -> OkAng a
        [_, (OkLen l), _] -> OkLen l
        [_, _, (OkPos g)] -> OkPos g
        _ -> Error ("Couldn't resolve parameter " ++ p)
  where
    r = map ($ p) [(readM readAngleM OkAng), (readM readLengthM OkLen), (readM readGeoPosM OkPos)]
eval (Antipode a) =
    case eval a of
        (OkPos p) -> OkPos (antipode p)
        r -> Error ("antipode: Expected 'Position' but was " ++ show r)
eval (Distance a b) =
    case [eval a, eval b] of
        [(OkPos p1), (OkPos p2)] -> OkLen (distance p1 p2)
        r -> Error ("distance: Expected 'Position' -> 'Position' but was " ++ show r)
eval (FinalBearing a b) =
    case [eval a, eval b] of
        [(OkPos p1), (OkPos p2)] -> OkAng (finalBearing p1 p2)
        r -> Error ("finalBearing: Expected 'Position' -> 'Position' but was " ++ show r)
eval (InitialBearing a b) =
    case [eval a, eval b] of
        [(OkPos p1), (OkPos p2)] -> OkAng (initialBearing p1 p2)
        r -> Error ("initialBearing: Expected 'Position' -> 'Position' but was " ++ show r)
eval (ReadGeo s) = maybe (Error ("Invalid position: " ++ s)) OkPos (readGeoPosM s)

readM :: (String -> Maybe a) -> (a -> Result) -> String -> Result
readM p r s = maybe (Error ("Invalid text: " ++ s)) r (p s)
