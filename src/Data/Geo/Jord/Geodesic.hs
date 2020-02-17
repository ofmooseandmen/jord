-- |
-- Module:      Data.Geo.Jord.Geodesic
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Solutions to the direct and inverse geodesic problems on ellipsoidal models using Vincenty formulaes.
-- A geodesic is the shortest path between two points on a curved surface - here an ellispoid. Using these
-- functions improves on the accuracy available using "Data.Geo.Jord.GreatCircle" at the expense of higher
-- CPU usage.
--
-- In order to use this module you should start with the following imports:
--
-- @
--     import Data.Geo.Jord.Geodesic
--     import Data.Geo.Jord.Position
-- @
--
-- If you wish to use both this module and the "Data.Geo.Jord.GreatCircle" module you must qualify both imports.
--
-- <http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf T Vincenty, "Direct and Inverse Solutions of Geodesics on the Ellipsoid with application of nested equations", Survey Review, vol XXIII no 176, 1975.>
--
module Data.Geo.Jord.Geodesic
    (
    -- * The 'Geodesic' type
      Geodesic
    , geodesicPos1
    , geodesicPos2
    , geodesicBearing1
    , geodesicBearing2
    , geodesicLength
    -- * Calculations
    , directGeodesic
    , inverseGeodesic
    , destination
    , finalBearing
    , initialBearing
    , surfaceDistance
    ) where

import Data.Geo.Jord.Internal
import Data.Geo.Jord.Position

-- | Geodesic line: shortest route between two positions on the surface of a model.
data Geodesic a =
    Geodesic
        { geodesicPos1 :: Position a -- ^ geodesic start position, p1.
        , geodesicPos2 :: Position a -- ^ geodesic end position, p2.
        , geodesicBearing1 :: Maybe Angle -- ^ initial bearing from p1 to p2, if p1 and p2 are different.
        , geodesicBearing2 :: Maybe Angle -- ^ final bearing from p1 to p2, if p1 and p2 are different
        , geodesicLength :: Length -- ^ length of the geodesic: the surface distance between p1 and p2.
        }
    deriving (Eq, Show)

-- | @directGeodesic p1 b1 d@ solves the direct geodesic problem using Vicenty formula: position
-- along the geodesic, reached from position @p1@ having travelled the __surface__ distance @d@ on
-- the initial bearing (compass angle) @b1@ at __constant__ height; it also returns the final bearing
-- at the reached position.
-- The Vincenty formula for the direct problem should always converge, however this function returns
-- 'Nothing' if it would ever fail to do so (probably thus indicating a bug in the implementation).
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Geodesic
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> directGeodesic (northPole WGS84) zero (kilometres 20003.931458623)
-- Just (Geodesic {geodesicPos1 = 90°0'0.000"N,0°0'0.000"E 0.0m (WGS84)
--               , geodesicPos2 = 90°0'0.000"S,180°0'0.000"E 0.0m (WGS84)
--               , geodesicBearing1 = Just 0°0'0.000"
--               , geodesicBearing2 = Just 180°0'0.000"
--               , geodesicLength = 20003.931458623km})
--
directGeodesic :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Geodesic a)
directGeodesic p1 b1 d
    | d == zero = Just (Geodesic p1 p1 (Just b1) (Just b1) zero)
    | otherwise =
        case rec of
            Nothing -> Nothing
            (Just (s, cosS, sinS, cos2S')) -> Just (Geodesic p1 p2 (Just b1) (Just b2) d)
                where x = sinU1 * sinS - cosU1 * cosS * cosAlpha1
                      lat2 =
                          atan2
                              (sinU1 * cosS + cosU1 * sinS * cosAlpha1)
                              ((1.0 - f) * sqrt (sinAlpha * sinAlpha + x * x))
                      lambda = atan2 (sinS * sinAlpha1) (cosU1 * cosS - sinU1 * sinS * cosAlpha1)
                      _C = f / 16.0 * cosSqAlpha * (4.0 + f * (4.0 - 3.0 * cosSqAlpha))
                      _L =
                          lambda -
                          (1.0 - _C) * f * sinAlpha *
                          (s + _C * sinS * (cos2S' + _C * cosS * (-1.0 + 2.0 * cos2S' * cos2S')))
                      lon2 = lon1 + _L
                      b2 = normalise (radians (atan2 sinAlpha (-x))) (decimalDegrees 360.0)
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

-- | @inverseGeodesic p1 p2@ solves the inverse geodesic problem using Vicenty formula: __surface__ distance,
-- and initial/final bearing between the geodesic line between positions @p1@ and @p2@.
-- The Vincenty formula for the inverse problem can fail to converge for nearly antipodal points in which
-- case this function returns 'Nothing'.
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Geodesic
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> inverseGeodesic (latLongPos 0 0 WGS84) (latLongPos 0.5 179.5 WGS84)
-- Just (Geodesic {geodesicPos1 = 0°0'0.000"N,0°0'0.000"E 0.0m (WGS84)
--               , geodesicPos2 = 0°30'0.000"N,179°30'0.000"E 0.0m (WGS84)
--               , geodesicBearing1 = Just 25°40'18.742"
--               , geodesicBearing2 = Just 154°19'37.507"
--               , geodesicLength = 19936.288578981km})
-- >>>
-- >>> inverseGeodesic (latLongPos 0 0 WGS84) (latLongPos 0.5 179.7 WGS84)
-- Nothing
--
inverseGeodesic :: (Ellipsoidal a) => Position a -> Position a -> Maybe (Geodesic a)
inverseGeodesic p1 p2
    | llEq p1 p2 = Just (Geodesic p1 p2 Nothing Nothing zero)
    | otherwise =
        case rec of
            Nothing -> Nothing
            (Just (cosL, sinL, s, cosS, sinS, sinSqS, cos2S', cosSqA)) ->
                Just (Geodesic p1 p2 (Just b1) (Just b2) d)
                where uSq = cosSqA * (a * a - b * b) / (b * b)
                      _A =
                          1 +
                          uSq / 16384.0 * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
                      _B = uSq / 1024.0 * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))
                      deltaSigma =
                          _B * sinS *
                          (cos2S' +
                           _B / 4.0 *
                           (cosS * (-1.0 + 2.0 * cos2S' * cos2S') -
                            _B / 6.0 * cos2S' * (-3.0 + 4.0 * sinS * sinS) *
                            (-3.0 + 4.0 * cos2S' * cos2S')))
                      d = metres (b * _A * (s - deltaSigma))
                      a1R =
                          if abs sinSqS < epsilon
                              then 0.0
                              else atan2 (cosU2 * sinL) (cosU1 * sinU2 - sinU1 * cosU2 * cosL)
                      a2R =
                          if abs sinSqS < epsilon
                              then pi
                              else atan2 (cosU1 * sinL) (-sinU1 * cosU2 + cosU1 * sinU2 * cosL)
                      b1 = normalise (radians a1R) (decimalDegrees 360.0)
                      b2 = normalise (radians a2R) (decimalDegrees 360.0)
  where
    lat1 = toRadians . latitude $ p1
    lon1 = toRadians . longitude $ p1
    lat2 = toRadians . latitude $ p2
    lon2 = toRadians . longitude $ p2
    ell = surface . model $ p1
    (a, b, f) = abf ell
    _L = lon2 - lon1 -- difference in longitude
    (_, cosU1, sinU1) = reducedLat lat1 f
    (_, cosU2, sinU2) = reducedLat lat2 f
    antipodal = abs _L > pi / 2.0 || abs (lat2 - lat1) > pi / 2.0
    rec = inverseRec _L cosU1 sinU1 cosU2 sinU2 _L f antipodal 0

-- | @finalBearing p1 p2@ computes the final bearing arriving at @p2@ from @p1@ in compass angle.
-- Compass angles are clockwise angles from true north: 0° = north, 90° = east, 180° = south, 270° = west.
-- The final bearing will differ from the initial bearing by varying degrees according to distance and latitude.
-- Returns 'Nothing' if both positions are equals or if 'inverseGeodesic' fails to converge.
--
-- This is equivalent to:
--
-- @
--     ('inverseGeodesic' p1 p2) >>= 'geodesicBearing2'
-- @
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Geodesic
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> p1 = latLongPos (-37.95103341666667) 144.42486788888888 WGS84
-- >>> p2 = latLongPos (-37.65282113888889) 143.92649552777777 WGS84
-- >>> initialBearing p1 p2
-- Just 307°10'25.070"
--
finalBearing :: (Ellipsoidal a) => Position a -> Position a -> Maybe Angle
finalBearing p1 p2 = inverseGeodesic p1 p2 >>= geodesicBearing2

-- | @initialBearing p1 p2@ computes the initial bearing from @p1@ to @p2@ in compass angle.
-- Compass angles are clockwise angles from true north: 0° = north, 90° = east, 180° = south, 270° = west.
-- Returns 'Nothing' if both positions are equals or if 'inverseGeodesic' fails to converge.
--
-- @
--     ('inverseGeodesic' p1 p2) >>= 'geodesicBearing1'
-- @
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Geodesic
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> p1 = latLongPos (-37.95103341666667) 144.42486788888888 WGS84
-- >>> p2 = latLongPos (-37.65282113888889) 143.92649552777777 WGS84
-- >>> initialBearing p1 p2
-- Just 306°52'5.373"
--
initialBearing :: (Ellipsoidal a) => Position a -> Position a -> Maybe Angle
initialBearing p1 p2 = inverseGeodesic p1 p2 >>= geodesicBearing1

-- | @surfaceDistance p1 p2@ computes the surface distance on the geodesic between the
-- positions @p1@ and @p2@.
-- This function relies on 'inverseGeodesic' and can therefore fail to compute the distance
-- for nearly antipodal positions.
--
-- @
--     fmap 'geodesicLength' ('inverseGeodesic' p1 p2)
-- @
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Geodesic
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> surfaceDistance (northPole WGS84) (southPole WGS84)
-- Just 20003.931458623km
--
surfaceDistance :: (Ellipsoidal a) => Position a -> Position a -> Maybe Length
surfaceDistance p1 p2 = fmap geodesicLength (inverseGeodesic p1 p2)

-- | @destination p b d@ computes the position along the geodesic, reached from
-- position @p@ having travelled the __surface__ distance @d@ on the initial bearing (compass angle) @b@
-- at __constant__ height.
-- Note that the  bearing will normally vary before destination is reached.
--
-- @
--     fmap 'geodesicPos2' ('directGeodesic' p b d)
-- @
--
-- ==== __Examples__
--
-- >>> import Data.Geo.Jord.Geodesic
-- >>> import Data.Geo.Jord.Position
-- >>>
-- >>> destination (wgs84Pos 54 154 (metres 15000)) (decimalDegrees 33) (kilometres 1000)
-- Just 61°10'8.983"N,164°7'52.258"E 15.0km (WGS84)
--
destination :: (Ellipsoidal a) => Position a -> Angle -> Length -> Maybe (Position a)
destination p b d = fmap geodesicPos2 (directGeodesic p b d)

directRec ::
       Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Int
    -> Maybe (Double, Double, Double, Double)
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
          _B / 6.0 * cos2Sigma' * (-3.0 + 4.0 * sinSigma * sinSigma) *
          (-3.0 + 4.0 * cos2Sigma' * cos2Sigma')))
    newSigma = dist / (b * _A) + deltaSigma

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
    -> Maybe (Double, Double, Double, Double, Double, Double, Double, Double)
inverseRec lambda cosU1 sinU1 cosU2 sinU2 _L f antipodal i
    | i == 1000 = Nothing
    -- co-incident/antipodal points (falls back on λ/σ = L)
    | sinSqSigma < epsilon = Just (inverseFallback cosL sinL sinSqSigma antipodal)
    | iterationCheck > pi = Nothing
    | abs (lambda - newLambda) <= 1e-12 =
        Just (cosL, sinL, sigma, cosSigma, sinSigma, sinSqSigma, cos2Sigma', cosSqAlpha)
    | otherwise = inverseRec newLambda cosU1 sinU1 cosU2 sinU2 _L f antipodal (i + 1)
  where
    sinL = sin lambda
    cosL = cos lambda
    sinSqSigma =
        (cosU2 * sinL) * (cosU2 * sinL) +
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
        (sigma +
         _C * sinSigma * (cos2Sigma' + _C * cosSigma * (-1.0 + 2.0 * cos2Sigma' * cos2Sigma')))
    iterationCheck =
        if antipodal
            then abs newLambda - pi
            else abs newLambda

inverseFallback ::
       Double
    -> Double
    -> Double
    -> Bool
    -> (Double, Double, Double, Double, Double, Double, Double, Double)
inverseFallback cosL sinL sinSqSigma antipodal =
    (cosL, sinL, sigma, cosSigma, sinSigma, sinSqSigma, cos2Sigma', cosSqAlpha)
  where
    sigma =
        if antipodal
            then pi
            else 0
    cosSigma =
        if antipodal
            then (-1)
            else 1
    sinSigma = 0
    cos2Sigma' = 1
    cosSqAlpha = 1

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
