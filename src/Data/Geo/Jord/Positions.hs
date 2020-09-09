-- |
-- Module:      Data.Geo.Jord.Positions
-- Copyright:   (c) 2020 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Functions to convert position between geodetic and geocentric and to transform position coordinates between ellipsoidal models.
--
-- @
-- import qualified Data.Geo.Jord.Geocentric as Geocentric
-- import qualified Data.Geo.Jord.Geodetic as Geodetic
-- import Data.Geo.Jord.Models
-- import qualified Data.Geo.Jord.Positions as Positions
-- import qualified Data.Geo.Jord.Transformations as Transformations
-- @
module Data.Geo.Jord.Positions
    (
    -- Geodetic <=> Geocentric
      toGeodetic
    , toGeocentric
    -- Coordinates transformation between ellipsoidal models
    , transform
    , transform'
    , transformAt
    , transformAt'
    ) where

import Data.Geo.Jord.Ellipsoid (Ellipsoid, eccentricity, equatorialRadius, isSphere, polarRadius)
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Length (Length)
import qualified Data.Geo.Jord.Length as Length
import qualified Data.Geo.Jord.Math3d as Math3d
import Data.Geo.Jord.Model
import Data.Geo.Jord.Tx (Graph, Params, Params15, Params7)
import qualified Data.Geo.Jord.Tx as Tx

-- | @toGeodetic p@ converts the geodetic coordinates of position @p@ to geocentric coordinates.
toGeodetic :: (Model m) => Geocentric.Position m -> Geodetic.Position m
toGeodetic p = Geodetic.atHeight (Geodetic.nvectorPos' nv (Geocentric.model p)) h
  where
    (nv, h) = nvectorFromGeocentric (Geocentric.metresCoords p) (surface . Geocentric.model $ p)

-- | @toGeocentric p@ converts the geocentric coordinates of position @p@ to geodetic coordinates.
toGeocentric :: (Model m) => Geodetic.Position m -> Geocentric.Position m
toGeocentric p =
    Geocentric.metresPos (Math3d.v3x c) (Math3d.v3y c) (Math3d.v3z c) (Geodetic.model' p)
  where
    c = nvectorToGeocentric (Geodetic.nvector p, Geodetic.height p) (surface . Geodetic.model' $ p)

-- | @transform p1 m2 g@ transforms the coordinates of the position @p1@ from its coordinate system into the coordinate
-- system defined by the model @m2@ using the graph @g@ to find the sequence of transformation parameters. Returns
-- 'Nothing' if the given graph does not contain a transformation from @m1@ to @m2@. For example:
--
-- >>> let pWGS84 = Positions.toGeocentric (Geodetic.latLongHeightPos 48.6921 6.1844 (Length.metres 188) WGS84)
-- >>> Positions.transform pWGS84 NAD83 Txs.fixed
-- Just (Position {gx = 4193.792080781km, gy = 454.433921298km, gz = 4768.166154789km, model = NAD83})
transform ::
       (Ellipsoidal a, Ellipsoidal b)
    => Geocentric.Position a
    -> b
    -> Graph Params7
    -> Maybe (Geocentric.Position b)
transform p1 m2 g = transformGraph p1 m2 g id

-- | @transform' p1 m2 tx@ transforms the coordinates of the position @p1@ from its coordinate system into the coordinate
-- system defined by the model @m2@ using the 7-parameters transformation @tx@. For example:
--
-- >>> let tx = Tx.params7 (995.6, -1910.3, -521.5) (-0.62) (25.915, 9.426, 11.599) -- WGS84 -> NAD83
-- >>> let pWGS84 = Positions.toGeocentric (Geodetic.latLongHeightPos 48.6921 6.1844 (Length.metres 188) WGS84)
-- >>> Positions.transform' pWGS84 NAD83 tx
-- Position {gx = 4193.792080781km, gy = 454.433921298km, gz = 4768.166154789km, model = NAD83}
transform' ::
       (Ellipsoidal a, Ellipsoidal b)
    => Geocentric.Position a
    -> b
    -> Params7
    -> Geocentric.Position b
transform' = transformOne

-- | @transformAt p1 e m2 g@ transforms the coordinates of the position @p1@ observed at epoch @e@ from its coordinate
-- system into the coordinate system defined by the model @m2@ using the graph @g@ to find the sequence of transformation
-- parameters. Returns 'Nothing' if the given graph does not contain a transformation from @m1@ to @m2@. For example:
--
-- >>> let pITRF2014 = Positions.toGeocentric (Geodetic.latLongHeightPos 48.6921 6.1844 (Length.metres 188) ITRF2014)
-- >>> Positions.transformAt pITRF2014 (Epoch 2019.0) NAD83_CORS96 Txs.timeDependent -- through ITRF2000
-- Just (Position {gx = 4193.791716941km, gy = 454.433860294km, gz = 4768.166466192km, model = NAD83_CORS96})
transformAt ::
       (EllipsoidalT0 a, EllipsoidalT0 b)
    => Geocentric.Position a
    -> Epoch
    -> b
    -> Graph Params15
    -> Maybe (Geocentric.Position b)
transformAt p1 e m2 g = transformGraph p1 m2 g (Tx.paramsAt e)

-- | @transformAt' p1 e m2 tx@ transforms the coordinates of the position @p1@ observed at epoch @e@ from its coordinate
-- system into the coordinate system defined by the model @m2@ using the 15-parameters transformation @tx@. For example:
--
-- >>> let tx7 = Tx.params7 (53.7, 51.2, -55.1) 1.2 (0.891, 5.39, -8.712)
-- >>> let txR = Tx.rates (0.1, 0.1, -1.9) 0.11 (0.81, 0.49, -0.792)
-- >>> let tx = Tx.Params15 (Epoch 2000.0) tx7 txR -- ITRF2014 -> ETRF2000
-- >>> let pITRF2014 = Positions.toGeocentric (Geodetic.latLongHeightPos 48.6921 6.1844 (Length.metres 188) ITRF2014)
-- >>> Positions.transformAt' pITRF2014 (Epoch 2019.0) ETRF2000 tx
-- Position {gx = 4193.791357037km, gy = 454.435390265km, gz = 4768.166475162km, model = ETRF2000}
transformAt' ::
       (EllipsoidalT0 a, EllipsoidalT0 b)
    => Geocentric.Position a
    -> Epoch
    -> b
    -> Params15
    -> Geocentric.Position b
transformAt' p1 e m2 ps = transformOne p1 m2 (Tx.paramsAt e ps)

-- | @nvectorToGeocentric (nv, h) e@ returns the geocentric coordinates equivalent to the given
-- /n/-vector @nv@ and height @h@ using the ellispoid @e@.
nvectorToGeocentric :: (Math3d.V3, Length) -> Ellipsoid -> Math3d.V3
nvectorToGeocentric (nv, h) e
    | isSphere e = nvectorToGeocentricS (nv, h) (equatorialRadius e)
    | otherwise = nvectorToGeocentricE (nv, h) e

nvectorToGeocentricS :: (Math3d.V3, Length) -> Length -> Math3d.V3
nvectorToGeocentricS (nv, h) r = Math3d.scale nv (Length.toMetres n)
  where
    n = Length.add h r

nvectorToGeocentricE :: (Math3d.V3, Length) -> Ellipsoid -> Math3d.V3
nvectorToGeocentricE (nv, h) e = Math3d.vec3 cx cy cz
  where
    nx = Math3d.v3x nv
    ny = Math3d.v3y nv
    nz = Math3d.v3z nv
    a = Length.toMetres . equatorialRadius $ e
    b = Length.toMetres . polarRadius $ e
    m = (a * a) / (b * b)
    n = b / sqrt ((nx * nx * m) + (ny * ny * m) + (nz * nz))
    h' = Length.toMetres h
    cx = n * m * nx + h' * nx
    cy = n * m * ny + h' * ny
    cz = n * nz + h' * nz

-- | @nvectorFromGeocentric g e@ returns the /n/-vector equivalent to the geocentric
-- coordinates @g@ using the ellispoid @e@.
nvectorFromGeocentric :: Math3d.V3 -> Ellipsoid -> (Math3d.V3, Length)
nvectorFromGeocentric g e
    | isSphere e = nvectorFromGeocentricS g (equatorialRadius e)
    | otherwise = nvectorFromGeocentricE g e

nvectorFromGeocentricS :: Math3d.V3 -> Length -> (Math3d.V3, Length)
nvectorFromGeocentricS g r = (Math3d.unit g, h)
  where
    h = Length.subtract (Length.metres (Math3d.norm g)) r

nvectorFromGeocentricE :: Math3d.V3 -> Ellipsoid -> (Math3d.V3, Length)
nvectorFromGeocentricE pv e = (nvecEllipsoidal d e2 k px py pz, Length.metres h)
  where
    px = Math3d.v3x pv
    py = Math3d.v3y pv
    pz = Math3d.v3z pv
    e' = eccentricity e
    e2 = e' * e'
    e4 = e2 * e2
    a = Length.toMetres . equatorialRadius $ e
    a2 = a * a
    p = (px * px + py * py) / a2
    q = ((1 - e2) / a2) * (pz * pz)
    r = (p + q - e4) / 6.0
    s = (e4 * p * q) / (4.0 * r * r * r)
    t = (1.0 + s + sqrt (s * (2.0 + s))) ** (1 / 3)
    u = r * (1.0 + t + 1.0 / t)
    v = sqrt (u * u + q * e4)
    w = e2 * (u + v - q) / (2.0 * v)
    k = sqrt (u + v + w * w) - w
    d = k * sqrt (px * px + py * py) / (k + e2)
    h = ((k + e2 - 1.0) / k) * sqrt (d * d + pz * pz)

nvecEllipsoidal :: Double -> Double -> Double -> Double -> Double -> Double -> Math3d.V3
nvecEllipsoidal d e2 k px py pz = Math3d.vec3 nx ny nz
  where
    s = 1.0 / sqrt (d * d + pz * pz)
    a = k / (k + e2)
    nx = s * a * px
    ny = s * a * py
    nz = s * pz

transformGraph ::
       (Ellipsoidal a, Ellipsoidal b, Params p)
    => Geocentric.Position a
    -> b
    -> Graph p
    -> (p -> Params7)
    -> Maybe (Geocentric.Position b)
transformGraph p1 m2 g f =
    case ps of
        [] -> Nothing
        _ -> Just (Geocentric.metresPos' v2 m2)
  where
    mi1 = modelId . Geocentric.model $ p1
    mi2 = modelId m2
    ps = Tx.paramsBetween mi1 mi2 g
    v2 = foldl (\gc p -> Tx.apply gc (f p)) (Geocentric.metresCoords p1) ps

transformOne ::
       (Ellipsoidal a, Ellipsoidal b)
    => Geocentric.Position a
    -> b
    -> Params7
    -> Geocentric.Position b
transformOne p1 m2 ps = Geocentric.metresPos' v2 m2
  where
    v2 = Tx.apply (Geocentric.metresCoords p1) ps
