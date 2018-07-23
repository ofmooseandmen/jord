{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Geo.Jord.Geodetics
    ( BearingDistance(..)
    , Geodetics(delta, destination)
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Positions
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Spherical
import Data.Geo.Jord.Transform

-- | Bearing and distance between 2 spherical positions.
newtype BearingDistance =
    BearingDistance (Angle, Length)

instance Norm BearingDistance Length where
    norm (BearingDistance (_, l)) = l

class (Norm c Length) =>
      Geodetics a b c
    where
    delta :: GeoPos a b -> GeoPos a b -> c
    destination :: GeoPos a b -> c -> GeoPos a b
    destination p0 d
        | isZero (norm d :: Length) = p0
        | otherwise = _destination p0 d
    -- private (not exported)
    _destination :: GeoPos a b -> c -> GeoPos a b

-- | Spherical geodetics calculations on 'NVector's.
instance Geodetics NVector Length BearingDistance where
    delta p1 p2 = BearingDistance (initialBearing p1 p2, surfaceDistance p1 p2)
    _destination (GeoPos v r) (BearingDistance (b, d)) = GeoPos (add (scale v (cos' ta)) (scale de (sin' ta))) r
      where
        ed = unit (cross northPole v) -- east direction vector at v
        nd = cross v ed -- north direction vector at v
        ta = central d r -- central angle
        de = add (scale nd (cos' b)) (scale ed (sin' b)) -- unit vector in the direction of the azimuth

-- | Spherical geodetics calculations on 'LatLong's.
instance Geodetics LatLong Length BearingDistance where
    delta p1 p2 = delta (toNVector p1) (toNVector p2)
    _destination p0 d = fromNVector 0.0 (_destination (toNVector p0) d)

-- | Spherical geodetics calculations on 'NVectorPosition's.
instance Geodetics NVectorPosition Length BearingDistance where
    delta p1 p2 = delta (toNVector p1) (toNVector p2)
    _destination p0 d = fromNVector (height p0) (_destination (toNVector p0) d)

-- | Spherical geodetics calculations on 'AngularPosition's.
instance Geodetics AngularPosition Length BearingDistance where
    delta p1 p2 = delta (toNVector p1) (toNVector p2)
    _destination p0 d = fromNVector (height p0) (_destination (toNVector p0) d)

-- | Spherical geodetics calculations on 'EcefPosition's.
instance Geodetics EcefPosition Length BearingDistance where
    delta p1 p2 = delta v1 v2
      where
        v1 = fromEcef p1 :: (GeoPos NVector Length)
        v2 = fromEcef p2 :: (GeoPos NVector Length)
    _destination p0 d = toEcef (_destination v d)
      where
        v = fromEcef p0 :: (GeoPos NVector Length)
