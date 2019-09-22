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
directGeodesic p1 b1 d =
    case rec of
        Nothing -> Nothing
        (Just (s, cosS, sinS, cos2S')) -> Just (p2, fb)
            where x = sinU1 * sinS - cosU1 * cosS * cosAlpha1
                  lat2 =
                      atan2 (sinU1 * cosS + cosU1 * sinS * cosAlpha1) ((1.0 - f) * sqrt (sinAlpha * sinAlpha + x * x))
                  lambda = atan2 (sinS * sinAlpha1) (cosU1 * cosS - sinU1 * sinS * cosAlpha1)
                  _C = f / 16.0 * cosSqAlpha * (4.0 + f * (4.0 - 3.0 * cosSqAlpha))
                  _L =
                      lambda -
                      (1.0 - _C) * f * sinAlpha *
                      (s + _C * sinS * (cos2S' + _C * cosS * (-1.0 + 2.0 * cos2S' * cos2S')))
                  lon2 = lon1 + _L
                  fb = normalise (radians (atan2 sinAlpha (-x))) (decimalDegrees 360.0)
                  p2 = latLongHeightPos' (radians lat2) (radians lon2) (height p1) (model p1)
  where
    lat1 = toRadians . latitude $ p1
    lon1 = toRadians . longitude $ p1
    ell = surface . model $ p1
    a = toMetres . equatorialRadius $ ell
    b = toMetres . polarRadius $ ell
    f = flattening ell
    br1 = toRadians b1
    cosAlpha1 = cos br1
    sinAlpha1 = sin br1
    (tanU1, cosU1, sinU1) = reduceLat lat1 f
    sigma1 = atan2 tanU1 cosAlpha1 -- angular distance on the sphere from the equator to p1
    sinAlpha = cosU1 * sinAlpha1 -- alpha = azimuth of the geodesic at the equator
    cosSqAlpha = 1.0 - sinAlpha * sinAlpha
    uSq = cosSqAlpha * (a * a - b * b) / (b * b)
    _A = 1.0 + uSq / 16384.0 * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
    _B = uSq / 1024.0 * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))
    dm = toMetres d
    sigma = dm / (b * _A)
    rec = directRec sigma1 dm _A _B b sigma 0

inverseGeodesic :: (Ellipsoidal a) => Position a -> Position a -> Maybe (Length, Angle, Angle)
inverseGeodesic p1 p2 = Nothing
  where
    lat1 = toRadians . latitude $ p1
    lon1 = toRadians . longitude $ p1
    lat2 = toRadians . latitude $ p2
    lon2 = toRadians . longitude $ p2
    ell = surface . model $ p1
    a = toMetres . equatorialRadius $ ell
    b = toMetres . polarRadius $ ell
    f = flattening ell
    _L = lon2 - lon1 -- difference in longitude
    (tanU1, cosU1, sinU1) = reduceLat lat1 f
    (tanU2, cosU2, sinU2) = reduceLat lat2 f
    antipodal = abs _L > pi / 2.0 || abs (lat2 - lat1) > pi / 2.0

geodesicDistance :: (Ellipsoidal a) => Position a -> Position a -> Maybe Length
geodesicDistance p1 p2 =
    case inverseGeodesic p1 p2 of
        Nothing -> Nothing
        (Just (d, _, _)) -> Just d

geodesicDestination :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a)
geodesicDestination p b d = fmap fst (directGeodesic p b d)

directRec :: Double -> Double -> Double -> Double -> Double -> Double -> Int -> Maybe (Double, Double, Double, Double)
directRec sigma1 dist _A _B b sigma i
    | i == 100 = Nothing
    | abs (sigma - newSigma) <= 1e-12 = Just (newSigma, cosSigma, sinSigma, cos2Sigma')
    | otherwise = directRec sigma1 dist _A _B b newSigma (i + 1)
  where
    cos2Sigma' = cos (2 * sigma1 + sigma)
    sinSigma = sin sigma
    cosSigma = cos sigma
    deltaSigma =
        _B * sinSigma *
        (cos2Sigma' +
         _B / 4.0 *
         (cosSigma * (-1.0 + 2.0 * cos2Sigma' * cos2Sigma') -
          _B / 6.0 * cos2Sigma' * (-3.0 + 4.0 * sinSigma * sinSigma) * (-3.0 + 4.0 * cos2Sigma' * cos2Sigma')))
    newSigma = dist / (b * _A) + deltaSigma

reduceLat :: Double -> Double -> (Double, Double, Double)
reduceLat lat f = (tanU, cosU, sinU)
  where
    tanU = (1.0 - f) * tan lat
    cosU = 1.0 / sqrt (1 + tanU * tanU)
    sinU = tanU * cosU