{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- | Spherical transformation: 'NVector' <-> 'EcefPosition'.
instance CTransform NVector Length where
    fromEcef (GeoPos p r) = GeoPos (fst (fromEcef' p r)) r
    toEcef (GeoPos v r) = GeoPos (toEcef' v 0.0 r) r

-- | Spherical transformation: 'LatLong' <-> 'EcefPosition'.
instance CTransform LatLong Length where
    fromEcef p = unvec 0.0 (fromEcef p :: (GeoPos NVector Length))
    toEcef = toEcef . vec

-- | Spherical transformation: 'NVectorPosition' <-> 'EcefPosition'.
instance CTransform NVectorPosition Length where
    fromEcef (GeoPos p r) = GeoPos (NVectorPosition nv h) r
      where
        (nv, h) = fromEcef' p r
    toEcef (GeoPos (NVectorPosition v h) r) = GeoPos (toEcef' v h r) r

-- | Spherical transformation: 'AngularPosition' <-> 'EcefPosition'.
instance CTransform AngularPosition Length where
    fromEcef (GeoPos p r) = GeoPos (AngularPosition (fromNVector nv) h) r
      where
        (nv, h) = fromEcef' p r
    toEcef = toEcef . vec

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
    initialBearing (GeoPos v1 _) (GeoPos v2 _) =
        normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360)
      where
        gc1 = cross v1 v2 -- great circle through p1 & p2
        gc2 = cross v1 northPole -- great circle through p1 & north pole
    -- | TODO : different radius ???
    _interpolate (GeoPos v0 r) (GeoPos v1 _) f = GeoPos (interpolate' v0 v1 f) r
    _insideSurface (GeoPos v _) ps =
        let aSum =
                foldl
                    (\a v' -> add a (uncurry angularDistance' v' (Just v)))
                    (decimalDegrees 0)
                    (egdes (map (sub v . pos) ps))
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
    _interpolate p0@(GeoPos (NVectorPosition _ h0) _) p1@(GeoPos (NVectorPosition _ h1) _) f =
        unvec h (_interpolate (vec p0) (vec p1) f)
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
    _interpolate p0@(GeoPos (AngularPosition _ h0) _) p1@(GeoPos (AngularPosition _ h1) _) f =
        unvec h (_interpolate (vec p0) (vec p1) f)
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
    height (GeoPos p r) = snd (fromEcef' p r)
    initialBearing p1 p2 = initialBearing v1 v2
      where
        v1 = fromEcef p1 :: (GeoPos NVector Length)
        v2 = fromEcef p2 :: (GeoPos NVector Length)
    _interpolate (GeoPos e0 r0) (GeoPos e1 r1) f = GeoPos (toEcef' iv h r0) r0
      where
        (v0, h0) = fromEcef' e0 r0
        (v1, h1) = fromEcef' e1 r1
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

toEcef' :: NVector -> Double -> Length -> EcefPosition
toEcef' v h r = EcefPosition ex' ey' ez'
  where
    uv = unit v
    a = toMetres r
    nx' = nx uv
    ny' = ny uv
    nz' = nz uv
    n = a / sqrt (nx' * nx' + ny' * ny' + nz' * nz')
    ex' = metres (n * nx' + h * nx')
    ey' = metres (n * ny' + h * ny')
    ez' = metres (n * nz' + h * nz')

fromEcef' :: EcefPosition -> Length -> (NVector, Double)
fromEcef' (EcefPosition x y z) r = (nvec d px py pz, h)
  where
    a = toMetres r
    a2 = a * a
    px = toMetres x
    py = toMetres y
    pz = toMetres z
    p = (px * px + py * py) / a2
    q = (1 / a2) * (pz * pz)
    r' = (p + q) / 6.0
    u = 2.0 * r'
    k = sqrt (u + u)
    d = k * sqrt (px * px + py * py) / k
    h = ((k - 1.0) / k) * sqrt (d * d + pz * pz)

nvec :: Double -> Double -> Double -> Double -> NVector
nvec d px py pz = nvector nx' ny' nz'
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    nx' = s * px
    ny' = s * py
    nz' = s * pz
