module Data.Geo.Jord.Spherical
    ( STransform(vec, unvec)
    , SGeodetics(angularDistance, antipode, destination, finalBearing,
           height, initialBearing, interpolate, insideSurface,
           surfaceDistance)
    ) where

import Data.Geo.Jord.Angle
import Data.Geo.Jord.Ellipsoid
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.NVector
import Data.Geo.Jord.Position (northPole)
import Data.Geo.Jord.Positions
import Data.Geo.Jord.Quantity

-- | geoAngularToEcef :: GeoPos AngularPosition -> GeoPos EcefPosition
-- | geoEcefToAngular :: GeoPos EcefPosition -> GeoPos AngularPosition
-- | angularToEcef :: AngularPosition -> Ellipsoid -> EcefPosition
-- | ecefToAngular :: EcefPosition -> Ellipsoid -> AngularPosition


-- | TODO : better functiona names.
class STransform a where
    vec :: GeoPos a -> GeoPos NVector
    unvec :: Double -> GeoPos NVector -> GeoPos a

instance STransform LatLong where
    vec (GeoPos ll e) = GeoPos (fromLatLong ll) e
    unvec _ (GeoPos nv e) = GeoPos (fromNVector nv) e

instance STransform NVectorPosition where
    vec (GeoPos (NVectorPosition nv _) e) = GeoPos nv e
    unvec h (GeoPos nv e) = GeoPos (NVectorPosition nv h) e

instance STransform AngularPosition where
    vec (GeoPos (AngularPosition ll _) e) = GeoPos (fromLatLong ll) e
    unvec h (GeoPos nv e) = GeoPos (AngularPosition (fromNVector nv) h) e

instance STransform EcefPosition where
    vec p = GeoPos (fst (fromEcef (pos p) e)) e
      where
        e = ellipsoid p
    unvec h (GeoPos nv e) = GeoPos (toEcef nv h e) e

-- TODO: add crossTrackDistance and intersections to this class
class (Eq a) => SGeodetics a where
    angularDistance :: GeoPos a -> GeoPos a -> Maybe (GeoPos a) -> Angle
    antipode :: GeoPos a -> GeoPos a
    destination :: GeoPos a -> Angle -> Length -> GeoPos a
    destination p b d
        | isZero d = p
        | otherwise = _destination p b d
    finalBearing :: GeoPos a -> GeoPos a -> Angle
    finalBearing p1 p2 = normalise (initialBearing p1 p2) (decimalDegrees 180)
    height :: GeoPos a -> Double
    initialBearing :: GeoPos a -> GeoPos a -> Angle
    interpolate :: GeoPos a -> GeoPos a -> Double -> GeoPos a
    interpolate p0 p1 f
        | f < 0 || f > 1 = error ("fraction must be in range [0..1], was " ++ show f)
        | f == 0 = p0
        | f == 1 = p1
        | otherwise = _interpolate p0 p1 f
    insideSurface :: GeoPos a -> [GeoPos a] -> Bool
    insideSurface p ps
        | null ps = False
        | head ps == last ps = insideSurface p (init ps)
        | length ps < 3 = False
        | otherwise = _insideSurface p ps
    surfaceDistance :: GeoPos a -> GeoPos a -> Length
    surfaceDistance p1 p2 = arcLength (angularDistance p1 p2 Nothing) (meanRadius (ellipsoid p1))
    -- private (hidden in export list)
    _destination :: GeoPos a -> Angle -> Length -> GeoPos a
    _insideSurface :: GeoPos a -> [GeoPos a] -> Bool
    _interpolate :: GeoPos a -> GeoPos a -> Double -> GeoPos a

instance SGeodetics NVector where
    angularDistance (GeoPos v1 _) (GeoPos v2 _) n = angularDistance' v1 v2 (fmap pos n)
    antipode (GeoPos v e) = GeoPos (scale v (-1.0)) e
    _destination (GeoPos v e) b d = GeoPos (add (scale v (cos' ta)) (scale de (sin' ta))) e
      where
        ed = unit (cross northPole v) -- east direction vector at v
        nd = cross v ed -- north direction vector at v
        ta = central d (meanRadius e) -- central angle
        de = add (scale nd (cos' b)) (scale ed (sin' b)) -- unit vector in the direction of the azimuth
    height _ = 0.0
    initialBearing (GeoPos v1 _) (GeoPos v2 _) =
        normalise (angularDistance' gc1 gc2 (Just v1)) (decimalDegrees 360)
      where
        gc1 = cross v1 v2 -- great circle through p1 & p2
        gc2 = cross v1 northPole -- great circle through p1 & north pole
    -- | TODO : different ellipsoid ???
    _interpolate (GeoPos v0 e) (GeoPos v1 _) f = GeoPos (unit (add v0 (scale (sub v1 v0) f))) e
    _insideSurface (GeoPos v _) ps =
        let aSum =
                foldl
                    (\a v' -> add a (uncurry angularDistance' v' (Just v)))
                    (decimalDegrees 0)
                    (egdes (map (sub v . pos) ps))
         in abs (toDecimalDegrees aSum) > 180.0

instance SGeodetics LatLong where
    angularDistance p1 p2 n = angularDistance (vec p1) (vec p2) (fmap vec n)
    antipode = unvec 0.0 . antipode . vec
    _destination p b d = unvec 0.0 (_destination (vec p) b d)
    height _ = 0.0
    initialBearing p1 p2 = initialBearing (vec p1) (vec p2)
    _interpolate p0 p1 f = unvec 0.0 (_interpolate (vec p0) (vec p1) f)
    _insideSurface p ps = _insideSurface (vec p) (fmap vec ps)

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

instance SGeodetics EcefPosition where
    angularDistance p1 p2 n = angularDistance (vec p1) (vec p2) (fmap vec n)
    antipode p = unvec (height p) . antipode . vec $ p
    _destination p b d = unvec (height p) (_destination (vec p) b d)
    height p = snd (fromEcef (pos p) (ellipsoid p))
    initialBearing p1 p2 = initialBearing (vec p1) (vec p2)
    _interpolate p0 p1 f =
        unvec h (_interpolate (vec p0) (vec p1) f)
      where
        h0 = height p0
        h1 = height p1
        h = h0 + (h1 - h0) * f
    _insideSurface p ps = _insideSurface (vec p) (fmap vec ps)

-- | private.
angularDistance' :: NVector -> NVector -> Maybe NVector -> Angle
angularDistance' v1 v2 n = atan2' sinO cosO
  where
    sign = maybe 1 (signum . dot (cross v1 v2)) n
    sinO = sign * norm (cross v1 v2)
    cosO = dot v1 v2

-- | [p1, p2, p3, p4] to [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
egdes :: [NVector] -> [(NVector, NVector)]
egdes ps = zip ps (tail ps ++ [head ps])

toEcef :: NVector -> Double -> Ellipsoid -> EcefPosition
toEcef v h e = EcefPosition ex' ey' ez'
  where
    uv = unit v
    a = toMetres (meanRadius e)
    nx' = nx uv
    ny' = ny uv
    nz' = nz uv
    n = a / sqrt (nx' * nx' + ny' * ny' + nz' * nz')
    ex' = metres (n * nx' + h * nx')
    ey' = metres (n * ny' + h * ny')
    ez' = metres (n * nz' + h * nz')

fromEcef :: EcefPosition -> Ellipsoid -> (NVector, Double)
fromEcef (EcefPosition x y z) e = (nvec d px py pz, h)
  where
    a = toMetres (meanRadius e)
    a2 = a * a
    px = toMetres x
    py = toMetres y
    pz = toMetres z
    p = (px * px + py * py) / a2
    q = (1 / a2) * (pz * pz)
    r = (p + q) / 6.0
    u = 2.0 * r
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
