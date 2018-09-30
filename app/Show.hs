-- |
-- Module:      Show
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Show 'Value's.
--
module Show
    ( showV
    ) where

import Data.Geo.Jord
import Data.List (intercalate)
import State

-- | show value.
showV :: Value -> State -> String
showV (Ang a) _ = "angle: " ++ showAng a
showV (Bool b) _ = show b
showV (Cpa c) s =
    "closest point of approach:" ++
    "\n      time    : " ++
    show (cpaTime c) ++
    "\n      distance: " ++
    showLength (cpaDistance c) s ++
    "\n      pos1    : " ++
    showLl (fromNVector . cpaPosition1 $ c :: LatLong) ++
    "\n      pos2    : " ++ showLl (fromNVector . cpaPosition2 $ c :: LatLong)
showV (Dlt d) s =
    "Delta:" ++
    "\n      x: " ++
    showLength (dx d) s ++
    "\n      y: " ++ showLength (dy d) s ++ "\n      z: " ++ showLength (dz d) s
showV (Dur d) _ = "duration: " ++ show d
showV (Double d) _ = show d
showV (Em m) _ = "Earth model: " ++ show m
showV (Ep p) s =
    "ECEF:" ++
    "\n      x: " ++
    showLength (ex p) s ++
    "\n      y: " ++ showLength (ey p) s ++ "\n      z: " ++ showLength (ez p) s
showV (FrmB y p r) _ =
    "Body (vehicle) frame:" ++
    "\n      yaw  : " ++
    showAng y ++ "\n      pitch: " ++ showAng p ++ "\n      roll : " ++ showAng r
showV (FrmL w) _ = "Local frame:" ++ "\n      wander azimuth: " ++ showAng w
showV FrmN _ = "North, East, Down frame"
showV (Ga ga) _ =
    "great arc: passing by " ++
    (showLl . nvectorToLatLong . gaStart $ ga) ++
    " and " ++ (showLl . nvectorToLatLong . gaEnd $ ga)
showV (Gc gc) _ =
    "great circle: passing by " ++
    (showLl . nvectorToLatLong . gcPos $ gc) ++ " heading on " ++ (showAng . gcBearing $ gc)
showV (Gp g) s = "latlong: " ++ showLl ll ++ "\n      height : " ++ showLength h s
  where
    ll = pos g
    h = height g
showV (Intp i) s =
    "intercept:" ++
    "\n      time               : " ++
    show (interceptTime i) ++
    "\n      distance           : " ++
    showLength (interceptDistance i) s ++
    "\n      pos                : " ++
    showLl (fromNVector . interceptPosition $ i :: LatLong) ++
    "\n      interceptor speed  : " ++
    showSpeed (interceptorSpeed i) s ++
    "\n      interceptor bearing: " ++ showAng (interceptorBearing i)
showV (Len l) s = "length: " ++ showLength l s
showV (Ned d) s =
    "NED:" ++
    "\n      north: " ++
    showLength (north d) s ++
    "\n      east : " ++ showLength (east d) s ++ "\n      down : " ++ showLength (down d) s
showV (Np nv) s =
    "n-vector: " ++
    show x ++ ", " ++ show y ++ ", " ++ show z ++ "\n      height  : " ++ showLength h s
  where
    v = vec (pos nv)
    x = vx v
    y = vy v
    z = vz v
    h = height nv
showV (Trk t) s =
    "track:" ++
    "\n      position: " ++
    showLl (fromNVector . trackPos $ t :: LatLong) ++
    "\n      height  : " ++
    showLength (height . trackPos $ t) s ++
    "\n      bearing : " ++
    showAng (trackBearing t) ++ "\n      speed   : " ++ showSpeed (trackSpeed t) s
showV (Spd spd) s = "speed: " ++ showSpeed spd s
showV (Vals []) _ = "empty"
showV (Vals vs) s = "\n  " ++ intercalate "\n\n  " (map (`showV` s) vs)

showAng :: Angle -> String
showAng a = show a ++ " (" ++ show (toDecimalDegrees a) ++ ")"

showLl :: LatLong -> String
showLl ll =
    show ll ++
    " (" ++
    show (toDecimalDegrees (latitude ll)) ++ ", " ++ show (toDecimalDegrees (longitude ll)) ++ ")"
