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
    ( Geodesic
    , geodesicP1
    , geodesicP2
    , geodesicLength
    , geodesicAzimuth1
    , geodesicAzimuth2
    , directGeodesic
    , inverseGeodesic
    , azimuths
    , geodesicDistance
    , geodesicDestination
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.Length
import Data.Geo.Jord.Model
import Data.Geo.Jord.Position

data Geodesic a =
    Geodesic
        { geodesicP1 :: Position a
        , geodesicP2 :: Position a
        , geodesicLength :: Length
        , geodesicAzimuth1 :: Angle
        , geodesicAzimuth2 :: Angle
        , iterations :: Int
        }
    deriving (Eq, Show)

directGeodesic :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Geodesic a)
directGeodesic p1 b1 d =
    case rec of
        Nothing -> Nothing
        (Just (s, cosS, sinS, cos2S', i)) -> Just (Geodesic p1 p2 d b1 fb i)
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
    (a, b, f) = abf ell
    br1 = toRadians b1
    cosAlpha1 = cos br1
    sinAlpha1 = sin br1
    (tanU1, cosU1, sinU1) = reducedLat lat1 f
    sigma1 = atan2 tanU1 cosAlpha1 -- angular distance on the sphere from the equator to p1
    sinAlpha = cosU1 * sinAlpha1 -- alpha = azimuth of the geodesic at the equator
    cosSqAlpha = 1.0 - sinAlpha * sinAlpha
    uSq = cosSqAlpha * (a * a - b * b) / (b * b)
    _A = 1.0 + uSq / 16384.0 * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
    _B = uSq / 1024.0 * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))
    dm = toMetres d
    sigma = dm / (b * _A)
    rec = directRec sigma1 dm _A _B b sigma 0

inverseGeodesic :: (Ellipsoidal a) => Position a -> Position a -> Maybe (Geodesic a)
inverseGeodesic p1 p2 = inverseGeodesic' p1 p2 (surface . model $ p1)

azimuths :: Position a -> Position a -> Ellipsoid -> Maybe (Angle, Angle)
azimuths p1 p2 ell = fmap (\g -> (geodesicAzimuth1 g, geodesicAzimuth2 g)) (inverseGeodesic' p1 p2 ell)

geodesicDistance :: (Ellipsoidal a) => Position a -> Position a -> Maybe Length
geodesicDistance p1 p2 = fmap geodesicLength (inverseGeodesic p1 p2)

geodesicDestination :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a)
geodesicDestination p b d = fmap geodesicP2 (directGeodesic p b d)

directRec ::
       Double -> Double -> Double -> Double -> Double -> Double -> Int -> Maybe (Double, Double, Double, Double, Int)
directRec sigma1 dist _A _B b sigma i
    | i == 100 = Nothing
    | abs (sigma - newSigma) <= 1e-12 = Just (newSigma, cosSigma, sinSigma, cos2Sigma', i)
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

inverseGeodesic' :: Position a -> Position a -> Ellipsoid -> Maybe (Geodesic a)
inverseGeodesic' p1 p2 ell =
    case rec of
        Nothing -> Nothing
        (Just (s, cosS, sinS, sinSqS, cos2S', cosSqA, i)) -> Just (Geodesic p1 p2 d a1 a2 i)
            where uSq = cosSqA * (a * a - b * b) / (b * b)
                  _A = 1 + uSq / 16384.0 * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
                  _B = uSq / 1024.0 * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))
                  deltaSigma =
                      _B * sinS *
                      (cos2S' +
                       _B / 4.0 *
                       (cosS * (-1.0 + 2.0 * cos2S' * cos2S') -
                        _B / 6.0 * cos2S' * (-3.0 + 4.0 * sinS * sinS) * (-3.0 + 4.0 * cos2S' * cos2S')))
                  d = metres (b * _A * (s - deltaSigma))
                  a1R =
                      if abs sinSqS < epsilon
                          then 0.0
                          else atan2 (cosU2 * sinS) (cosU1 * sinU2 - sinU1 * cosU2 * cosS)
                  a2R =
                      if abs sinSqS < epsilon
                          then pi
                          else atan2 (cosU1 * sinS) (-sinU1 * cosU2 + cosU1 * sinU2 * cosS)
                  a1 = normalise (radians a1R) (decimalDegrees 360.0)
                  a2 = normalise (radians a2R) (decimalDegrees 360.0)
  where
    lat1 = toRadians . latitude $ p1
    lon1 = toRadians . longitude $ p1
    lat2 = toRadians . latitude $ p2
    lon2 = toRadians . longitude $ p2
    (a, b, f) = abf ell
    _L = lon2 - lon1 -- difference in longitude
    (_, cosU1, sinU1) = reducedLat lat1 f
    (_, cosU2, sinU2) = reducedLat lat2 f
    antipodal = abs _L > pi / 2.0 || abs (lat2 - lat1) > pi / 2.0
    rec = inverseRec _L cosU1 sinU1 cosU2 sinU2 _L f antipodal 0

inverseRec ::
       Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Bool
    -> Int
    -> Maybe (Double, Double, Double, Double, Double, Double, Int)
inverseRec lambda cosU1 sinU1 cosU2 sinU2 _L f antipodal i
    | i == 1000 = Nothing
    | sinSqSigma < epsilon = Nothing -- TODO, co-incident/antipodal points (falls back on λ/σ = L)
    | iterationCheck > pi = Nothing
    | abs (lambda - newLambda) <= 1e-12 = Just (sigma, cosSigma, sinSigma, sinSqSigma, cos2Sigma', cosSqAlpha, i)
    | otherwise = inverseRec newLambda cosU1 sinU1 cosU2 sinU2 _L f antipodal (i + 1)
  where
    sinL = sin lambda
    cosL = cos lambda
    sinSqSigma =
        (cosU2 * sinL) + (cosU2 * sinL) +
        (cosU1 * sinU2 - sinU1 * cosU2 * cosL) * (cosU1 * sinU2 - sinU1 * cosU2 * cosL)
    sinSigma = sqrt sinSqSigma
    cosSigma = sinU1 * sinU2 + cosU1 * cosU2 * cosL
    sigma = atan2 sinSigma cosSigma
    sinAlpha = cosU1 * cosU2 * sinL / sinSigma
    cosSqAlpha = 1 - sinAlpha * sinAlpha
    cos2Sigma' =
        if cosSqAlpha /= 0
            then cosSigma - 2.0 * sinU1 * sinU2 / cosSqAlpha
            else 0
    _C = f / 16.0 * cosSqAlpha * (4.0 + f * (4.0 - 3.0 * cosSqAlpha))
    newLambda =
        _L +
        (1.0 - _C) * f * sinAlpha *
        (sigma + _C * sinSigma * (cos2Sigma' + _C * cosSigma * (-1.0 + 2.0 * cos2Sigma' * cos2Sigma')))
    iterationCheck =
        if antipodal
            then abs newLambda - pi
            else abs newLambda

-- | see Numeric.Limits
epsilon :: Double
epsilon = r
  where
    r = 1 - encodeFloat (m - 1) e
    (m, e) = decodeFloat (1 :: Double)

reducedLat :: Double -> Double -> (Double, Double, Double)
reducedLat lat f = (tanU, cosU, sinU)
  where
    tanU = (1.0 - f) * tan lat
    cosU = 1.0 / sqrt (1 + tanU * tanU)
    sinU = tanU * cosU

abf :: Ellipsoid -> (Double, Double, Double)
abf e = (toMetres . equatorialRadius $ e, toMetres . polarRadius $ e, flattening e)