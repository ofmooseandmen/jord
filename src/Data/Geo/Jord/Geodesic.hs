-- |
-- Module:      Data.Geo.Jord.Geodesic
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- TODO
-- <http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf T Vincenty, "Direct and Inverse Solutions of Geodesics on the Ellipsoid with application of nested equations", Survey Review, vol XXIII no 176, 1975.>
--
module Data.Geo.Jord.Geodesic
    ( directGeodesic
    , inverseGeodesic
    , geodesicDistance
    , geodesicDestination
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Length
import Data.Geo.Jord.Model
import Data.Geo.Jord.Position

directGeodesic :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a, Angle)
directGeodesic p1 b1 d = Nothing
  where
    lat = toRadians . latitude $ p1
    lon = toRadians . longitude $ p1
    ell = surface . model $ p1
    a = toMetres . equatorialRadius $ ell
    b = toMetres . polarRadius $ ell
    f = flattening ell
    br1 = toRadians b1
    cosAlpha1 = cos br1
    sinAlpha1 = sin br1
    tanU1 = (1.0 - f) * tan lat
    cosU1 = 1.0 / sqrt (1 + tanU1 * tanU1)
    sinU1 = tanU1 / cosU1
    sigma1 = atan2 tanU1 cosAlpha1 -- angular distance on the sphere from the equator to p1
    sinAlpha = cosU1 * sinAlpha1 -- alpha = azimuth of the geodesic at the equator
    cosSqAlpha = 1.0 - sinAlpha * sinAlpha
    uSq = cosSqAlpha * (a * a - b * b) / (b * b)
    _A = 1.0 + uSq / 16384.0 * (4096.0 + uSq * (-768 + uSq * (320.0 - 175.0 * uSq)))
    _B = uSq / 1024.0 * (256 + uSq * (-128 + uSq * (74 - 47 * uSq)))

inverseGeodesic :: (Ellipsoidal a) => Position a -> Position a -> Maybe (Length, Angle, Angle)
inverseGeodesic _ _ = Nothing

geodesicDistance :: (Ellipsoidal a) => Position a -> Position a -> Maybe Length
geodesicDistance _ _ = Nothing

geodesicDestination :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a)
geodesicDestination p b d = fmap fst (directGeodesic p b d)
{-
directIt :: Double -> Int -> Maybe Void
directIt _ _ = Nothing
  where
    cos2Sigma' = cos (2 * sigma1 + sigma)
    sinSigma = sin sigma
    cosSigma = cos sigma
    deltaSigma =
        _B * sinSigma *
        (cos2Sigma' +
         _B / 4.0 *
         (cosSigma * (-1.0 + 2.0 + cos2Sigma' * cos2Sigma') -
          _B / 60 * cos2Sigma' * (-3.0 + 4.0 * sinSigma * sinSigma) *
          (-3.0 + 4.0 * cos2Sigma' * cos2Sigma')))
    newSigma = dist / (b * _A) + deltaSigma
-}
