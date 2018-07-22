module Data.Geo.Jord.Spherical
    ( SGeodetics(angularDistance, antipode, destination, finalBearing,
           height, initialBearing, interpolate, insideSurface,
           surfaceDistance)
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Positions
import Data.Geo.Jord.Quantity
import Data.Geo.Jord.Transform

-- TODO: add crossTrackDistance and intersections to this class
-- | Spherical geodetics calculations.
class (Eq a) => SGeodetics a where
    angularDistance :: GeoPos a Length -> GeoPos a Length -> Maybe (GeoPos a Length) -> Angle
    antipode :: GeoPos a Length -> GeoPos a Length
    destination :: GeoPos a Length -> Angle -> Length -> GeoPos a Length
    destination p b d
        | isZero d = p
        | otherwise = _destination p b d
    finalBearing :: GeoPos a Length -> GeoPos a Length -> Angle
    finalBearing p1 p2 = normalise (initialBearing p1 p2) (decimalDegrees 180)
    height :: GeoPos a Length -> Double
    initialBearing :: GeoPos a Length -> GeoPos a Length -> Angle
    interpolate :: GeoPos a Length -> GeoPos a Length -> Double -> GeoPos a Length
    interpolate p0 p1 f
        | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
        | f == 0 = p0
        | f == 1 = p1
        | otherwise = _interpolate p0 p1 f
    insideSurface :: GeoPos a Length -> [GeoPos a Length] -> Bool
    insideSurface p ps
        | null ps = False
        | head ps == last ps = insideSurface p (init ps)
        | length ps < 3 = False
        | otherwise = _insideSurface p ps
    surfaceDistance :: GeoPos a Length -> GeoPos a Length -> Length
    surfaceDistance p1 p2 = arcLength (angularDistance p1 p2 Nothing) (model p1)
    -- private (not exported)
    _destination :: GeoPos a Length -> Angle -> Length -> GeoPos a Length
    _insideSurface :: GeoPos a Length -> [GeoPos a Length] -> Bool
    _interpolate :: GeoPos a Length -> GeoPos a Length -> Double -> GeoPos a Length

-- | Spherical geodetics calculations on 'NVector's.
instance SGeodetics NVector where
    angularDistance (GeoPos v1 _) (GeoPos v2 _) n = angularDistance' v1 v2 (fmap pos n)
    antipode (GeoPos v r) = GeoPos (scale v (-1.0)) r
    _destination (GeoPos v r) b d = GeoPos (add (scale v (cos' ta)) (scale de (sin' ta))) r
      where
        ed = unit (cross northPole v) -- east direction vector at v
        nd = cross v ed -- north direction vector at v
        ta = central d r -- central angle
        de = add (scale nd (cos' b)) (scale ed (sin' b)) -- unit vector in the direction of the azimuth
    height _ = 0.0
    initialBearing (GeoPos v1 _) (GeoPos v2 _) = normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360)
      where
        gc1 = cross v1 v2 -- great circle through p1 & p2
        gc2 = cross v1 northPole -- great circle through p1 & north pole
    -- | TODO : different radius ???
    _interpolate (GeoPos v0 r) (GeoPos v1 _) f = GeoPos (interpolate' v0 v1 f) r
    _insideSurface (GeoPos v _) ps =
        let aSum = foldl (\a v' -> add a (uncurry angularDistance' v' (Just v))) (decimalDegrees 0) (egdes (map (sub v . pos) ps))
         in abs (toDecimalDegrees aSum) > 180.0

-- | Spherical geodetics calculations on 'LatLong's.
instance SGeodetics LatLong where
    angularDistance p1 p2 n = angularDistance (vec p1) (vec p2) (fmap vec n)
    antipode = unvec 0.0 . antipode . vec
    _destination p b d = unvec 0.0 (_destination (vec p) b d)
    height _ = 0.0
    initialBearing p1 p2 = initialBearing (vec p1) (vec p2)
    _interpolate p0 p1 f = unvec 0.0 (_interpolate (vec p0) (vec p1) f)
    _insideSurface p ps = _insideSurface (vec p) (fmap vec ps)

-- | Spherical geodetics calculations on 'NVectorPosition's.
instance SGeodetics NVectorPosition where
    angularDistance p1 p2 n = angularDistance (vec p1) (vec p2) (fmap vec n)
    antipode p = unvec (height p) . antipode . vec $ p
    _destination p b d = unvec (height p) (_destination (vec p) b d)
    height (GeoPos (NVectorPosition _ h) _) = h
    initialBearing p1 p2 = initialBearing (vec p1) (vec p2)
    _interpolate p0@(GeoPos (NVectorPosition _ h0) _) p1@(GeoPos (NVectorPosition _ h1) _) f = unvec h (_interpolate (vec p0) (vec p1) f)
      where
        h = h0 + (h1 - h0) * f
    _insideSurface p ps = _insideSurface (vec p) (fmap vec ps)

-- | Spherical geodetics calculations on 'AngularPosition's.
instance SGeodetics AngularPosition where
    angularDistance p1 p2 n = angularDistance (vec p1) (vec p2) (fmap vec n)
    antipode p = unvec (height p) . antipode . vec $ p
    _destination p b d = unvec (height p) (_destination (vec p) b d)
    height (GeoPos (AngularPosition _ h) _) = h
    initialBearing p1 p2 = initialBearing (vec p1) (vec p2)
    _interpolate p0@(GeoPos (AngularPosition _ h0) _) p1@(GeoPos (AngularPosition _ h1) _) f = unvec h (_interpolate (vec p0) (vec p1) f)
      where
        h = h0 + (h1 - h0) * f
    _insideSurface p ps = _insideSurface (vec p) (fmap vec ps)

-- | Spherical geodetics calculations on 'EcefPosition's.
instance SGeodetics EcefPosition where
    angularDistance p1 p2 n = angularDistance v1 v2 vn
      where
        v1 = fromEcef p1 :: (GeoPos NVector Length)
        v2 = fromEcef p2 :: (GeoPos NVector Length)
        vn = fmap (\p -> fromEcef p :: (GeoPos NVector Length)) n
    antipode p = toEcef . antipode $ v
      where
        v = fromEcef p :: (GeoPos NVectorPosition Length)
    _destination p b d = toEcef (_destination v b d)
      where
        v = fromEcef p :: (GeoPos NVectorPosition Length)
    height (GeoPos p r) = snd (fromEcefSpherical p r)
    initialBearing p1 p2 = initialBearing v1 v2
      where
        v1 = fromEcef p1 :: (GeoPos NVector Length)
        v2 = fromEcef p2 :: (GeoPos NVector Length)
    _interpolate (GeoPos e0 r0) (GeoPos e1 r1) f = GeoPos (toEcefSpherical iv h r0) r0
      where
        (v0, h0) = fromEcefSpherical e0 r0
        (v1, h1) = fromEcefSpherical e1 r1
        h = h0 + (h1 - h0) * f
        iv = interpolate' v0 v1 f
    _insideSurface p ps = _insideSurface v vs
      where
        v = fromEcef p :: (GeoPos NVector Length)
        vs = fmap (\p' -> fromEcef p' :: (GeoPos NVector Length)) ps

-------------
-- private --
-------------
angularDistance' :: NVector -> NVector -> Maybe NVector -> Angle
angularDistance' v1 v2 n = atan2' sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2

interpolate' :: NVector -> NVector -> Double -> NVector
interpolate' v0 v1 f = unit (add v0 (scale (sub v1 v0) f))

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [NVector] -> [(NVector, NVector)]
egdes ps = zip ps (tail ps ++ [head ps])
