# Jord - Geographical Position Calculations

[![travis build status](https://img.shields.io/travis/ofmooseandmen/jord/master.svg?label=travis+build)](https://travis-ci.org/ofmooseandmen/jord)
[![Hackage](https://img.shields.io/hackage/v/jord.svg)](http://hackage.haskell.org/package/jord)
[![license](https://img.shields.io/badge/license-BSD3-lightgray.svg)](https://opensource.org/licenses/BSD-3-Clause)

> __Jord__ [_Swedish_] is __Earth__ [_English_]

## What is this?

Jord is a [Haskell](https://www.haskell.org) library that implements various geographical position calculations using the algorithms described in [Gade, K. (2010) - A Non-singular Horizontal Position Representation](http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf),
[Shudde, Rex H. (1986) - Some tactical algorithms for spherical geometry](https://calhoun.nps.edu/bitstream/handle/10945/29516/sometacticalalgo00shud.pdf) and [Vincenty, T. (1975) - Direct and Inverse Solutions of Geodesics on the Ellipsoid](https://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf):

- conversion between ECEF (earth-centred, earth-fixed), latitude/longitude and [*n*-vector](https://www.navlab.net/nvector) positions for spherical and ellipsoidal earth model,
- conversion between latitude/longitude and *n*-vector positions,
- local, body and north, east, down Frames: delta between positions, target position from reference position and delta,
- great circles: surface distance, initial & final bearing, interpolated position, great circle intersections, cross track distance, ...,
- geodesic: surface distance, initial & final bearing and destination,
- kinematics: position from p0, bearing and speed, closest point of approach between tracks, intercept (time, speed, minimum speed),
- transformation between coordinate systems (both fixed and time-dependent).

## How do I build it?

If you have [Stack](https://docs.haskellstack.org/en/stable/README/),
then:
```sh
$ stack build --test
```

## How do I use it?

[See documentation on Hackage](http://hackage.haskell.org/package/jord/docs/Data-Geo-Jord.html)

## Solutions to the 10 examples from [NavLab](https://www.navlab.net/nvector)

### Example 1: A and B to delta

*Given two positions, A and B as latitudes, longitudes and depths relative to Earth, E.*
 
*Find the exact vector between the two positions, given in meters north, east, and down, and find the direction (azimuth) to B, relative to north. Assume WGS-84 ellipsoid. The given depths are from the ellipsoid surface. Use position A to define north, east, and down directions. (Due to the curvature of Earth and different directions to the North Pole, the north, east, and down directions will change (relative to Earth) for different places. A must be outside the poles for the north and east directions to be defined.)*

```haskell
import Data.Geo.Jord.LocalFrames

posA = wgs84Pos 1 2 (metres 3)
posB = wgs84Pos 4 5 (metres 6)

delta = nedBetween posA posB 
-- > Ned (Vector3d {vx = 331730.234781, vy = 332997.874989, vz = 17404.271362})
slantRange delta 
-- > 470.356717903km
bearing delta 
-- > 45°6'33.347"
elevation delta 
-- > -2°7'14.011"
```

### Example 2: A and B to delta

*A radar or sonar attached to a vehicle B (Body coordinate frame) measures the distance and direction to an object C. We assume that the distance and two angles (typically bearing and elevation relative to B) are already combined to the vector p_BC_B (i.e. the vector from B to C, decomposed in B). The position of B is given as n_EB_E and z_EB, and the orientation (attitude) of B is given as R_NB (this rotation matrix can be found from roll/pitch/yaw by using zyx2R).*
 
*Find the exact position of object C as n-vector and depth ( n_EC_E and z_EC ), assuming Earth ellipsoid with semi-major axis a and flattening f. For WGS-72, use a = 6 378 135 m and f = 1/298.26.*

```haskell
import Data.Geo.Jord.LocalFrames

f = frameB (decimalDegrees 40) (decimalDegrees 20) (decimalDegrees 30)
p = nvectorHeightPos 1 2 3 (metres 400) WGS72
d = deltaMetres 3000 2000 100

target p f d
-- > 53°18'46.839"N,63°29'6.179"E 406.006018m (WGS72)
```

### Example 3: ECEF-vector to geodetic latitude

*Position B is given as an “ECEF-vector” p_EB_E (i.e. a vector from E, the center of the Earth, to B, decomposed in E). Find the geodetic latitude, longitude and height (latEB, lonEB and hEB), assuming WGS-84 ellipsoid.*

```haskell
import Data.Geo.Jord.Position

geocentricMetresPos 5733900.0 (-6371000.0) 7008100.000000001 WGS84
-- > 39°22'43.495"N,48°0'46.035"W 4702.059834295km (WGS84)
```

### Example 4: Geodetic latitude to ECEF-vector

*Geodetic latitude, longitude and height are given for position B as latEB, lonEB and hEB, find the ECEF-vector for this position, p_EB_E.*

```haskell
import Data.Geo.Jord.Position

gcvec (wgs84Pos 1 2 (metres 3))
-- > Vector3d {vx = 6373290.277218281, vy = 222560.20067473655, vz = 110568.82718177968}
```

### Example 5: Surface distance

*Find the surface distance sAB (i.e. great circle distance) between two positions A and B. The heights of A and B are ignored, i.e. if they don’t have zero height, we seek the distance between the points that are at the surface of the Earth, directly above/below A and B. The Euclidean distance (chord length) dAB should also be found. Use Earth radius 6371e3 m. Compare the results with exact calculations for the WGS-84 ellipsoid.*

```haskell
import Data.Geo.Jord.GreatCircle

posA = s84Pos 88 0 zero
posB = s84Pos 89 (-170) zero

surfaceDistance posA posB
-- > 332.456901835km
```

*Exact solution for the WGS84 ellipsoid*

```haskell
import Data.Geo.Jord.Geodesic

posA = wgs84Pos 88 0 zero
posB = wgs84Pos 89 (-170) zero

surfaceDistance posA posB
-- > Just 333.947509469km
```

### Example 6: Interpolated position

*Given the position of B at time t0 and t1, n_EB_E(t0) and n_EB_E(t1).*
 
*Find an interpolated position at time ti, n_EB_E(ti). All positions are given as n-vectors.*

```haskell
import Data.Geo.Jord.GreatCircle

posA = s84Pos 89 0 zero
posB = s84Pos 89 180 zero
f = (16 - 10) / (20 - 10) :: Double

interpolate posA posB f
-- > 89°47'59.929"N,180°0'0.000"E 0.0m (S84)
```

### Example 7: Mean position

*Three positions A, B, and C are given as n-vectors n_EA_E, n_EB_E, and n_EC_E. Find the mean position, M, given as n_EM_E. Note that the calculation is independent of the depths of the positions.*

```haskell
import Data.Geo.Jord.GreatCircle

ps = [s84Pos 90 0 zero, s84Pos 60 10 zero, s84Pos 50 (-20) zero]

mean ps
-- > Just 67°14'10.150"N,6°55'3.040"W 0.0m (S84)
```

### Example 8: A and azimuth/distance to B

*We have an initial position A, direction of travel given as an azimuth (bearing) relative to north (clockwise), and finally the distance to travel along a great circle given as sAB. Use Earth radius 6371e3 m to find the destination point B.*
 
*In geodesy this is known as “The first geodetic problem” or “The direct geodetic problem” for a sphere, and we see that this is similar to Example 2, but now the delta is given as an azimuth and a great circle distance. (“The second/inverse geodetic problem” for a sphere is already solved in Examples 1 and 5.)*

```haskell
import Data.Geo.Jord.GreatCircle

p = s84Pos 80 (-90) zero

destination p (decimalDegrees 200) (metres 1000)
-- > 79°59'29.575"N,90°1'3.714"W 0.0m (S84)
```

*Exact solution for the WGS84 ellipsoid*

```haskell
import Data.Geo.Jord.Geodesic

p = wgs84Pos 80 (-90) zero

destination p (decimalDegrees 200) (metres 1000)
-- > Just 79°59'29.701"N,90°1'3.436"W 0.0m (WGS84)
```
