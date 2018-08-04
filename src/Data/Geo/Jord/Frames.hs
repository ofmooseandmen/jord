module Data.Geo.Jord.Frames
    ( FrameN(..)
    , FrameB(..)
    , FrameL(..)
    , Rotation(..)
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Vector3d

data FrameN =
    FrameN
    deriving (Eq, Show)

data FrameB = FrameB
    { yaw :: Angle
    , pitch :: Angle
    , roll :: Angle
    } deriving (Eq, Show)

data FrameL = FrameL
    { wanderAzimuth :: Angle
    } deriving (Eq, Show)

class Rotation a where
  earthToFrame :: Vector3d -> a -> [Vector3d]
  frameToEarth :: Vector3d -> a -> [Vector3d]
  frameToEarth v f = transpose (earthToFrame v f)

instance Rotation FrameN where
  earthToFrame v _ = rm
    where
      np = vec northPole
      rd = vscale v (-1) -- down (pointing opposite to n-vector)
      re = vunit (vcross np v) -- east (pointing perpendicular to the plane)
      rn = vcross re rd -- north (by right hand rule)
      rm = [rn, re, rd]

-- | transpose matrix made of 'Vector3d'.
transpose :: [Vector3d] -> [Vector3d]
transpose m = fmap l2v (transpose' xs)
   where
     xs = fmap v2l m

-- | transpose matrix.
transpose' :: [[Double]] -> [[Double]]
transpose' ([]:_) = []
transpose' x = (map head x) : transpose' (map tail x)

-- | 'Vector3d' to list of doubles.
v2l :: Vector3d -> [Double]
v2l (Vector3d x' y' z') = [x', y', z']

-- | list of doubles to 'Vector3d'.
l2v :: [Double] -> Vector3d
l2v ([x', y', z']) = Vector3d x' y' z'
l2v xs = error ("Invalid list: " ++ (show xs))
