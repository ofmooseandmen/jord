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
- local frames (body; local level, wander azimuth; north, east, down): delta between positions, target position from reference position and delta,
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

Import the core functional modules as qualified:

```haskell
import qualified Data.Geo.Jord.Angle as Angle
import qualified Data.Geo.Jord.Duration as Duration
import qualified Data.Geo.Jord.Geocentric as Geocentric
import qualified Data.Geo.Jord.Geodetic as Geodetic
import qualified Data.Geo.Jord.Geodesic as Geodesic
import qualified Data.Geo.Jord.GreatCircle as GreatCircle
import qualified Data.Geo.Jord.Kinematics as Kinematics
import qualified Data.Geo.Jord.Length as Length
import qualified Data.Geo.Jord.LocalFrames as LocalFrames
import qualified Data.Geo.Jord.Positions as Positions
import qualified Data.Geo.Jord.Speed as Speed
```

Import data types and models and transformation parameters:

```haskell
import Data.Geo.Jord.Angle(Angle)
import Data.Geo.Jord.Kinematics(Track(..))
import Data.Geo.Jord.Length(Length)
import Data.Geo.Jord.LocalFrames(Ned(..), Delta(..))
import Data.Geo.Jord.Models
import Data.Geo.Jord.Speed(Speed)
```

## Solutions to the 10 examples from [NavLab](https://www.navlab.net/nvector)

### Example 1: A and B to delta

*Given two positions, A and B as latitudes, longitudes and depths relative to Earth, E.*

*Find the exact vector between the two positions, given in meters north, east, and down, and find the direction (azimuth)
to B, relative to north. Assume WGS-84 ellipsoid. The given depths are from the ellipsoid surface. Use position A to
define north, east, and down directions. (Due to the curvature of Earth and different directions to the North Pole,
the north, east, and down directions will change (relative to Earth) for different places. A must be outside the poles
for the north and east directions to be defined.)*

```haskell
example1 :: IO()
example1 = do
    let pA = Geodetic.wgs84Pos 1 2 (Length.metres 3)
    let pB = Geodetic.wgs84Pos 4 5 (Length.metres 6)

    let ned = LocalFrames.nedBetween pA pB
    -- Ned {north = 331.730863099km, east = 332.998501491km, down = 17.39830421km}

    let slantRange = LocalFrames.slantRange ned
    -- 470.357383823km

    let bearing = LocalFrames.bearing ned
    -- 45°6'33.346"

    let elevation = LocalFrames.elevation ned
    -- -2°7'11.381"

    putStrLn ("NavLab, Example1: A and B to delta\n\
              \  delta      = " ++ (show ned) ++ "\n\
              \  slantRange = " ++ (show slantRange) ++ "\n\
              \  bearing    = " ++ (show bearing) ++ "\n\
              \  elevation  = " ++ (show elevation) ++ "\n")
```

### Example 2: B and delta to C

*A radar or sonar attached to a vehicle B (Body coordinate frame) measures the distance and direction to an object C.
We assume that the distance and two angles (typically bearing and elevation relative to B) are already combined to the
vector p_BC_B (i.e. the vector from B to C, decomposed in B). The position of B is given as n_EB_E and z_EB, and the
orientation (attitude) of B is given as R_NB (this rotation matrix can be found from roll/pitch/yaw by using zyx2R).*

*Find the exact position of object C as n-vector and depth ( n_EC_E and z_EC ), assuming Earth ellipsoid with semi-major
axis a and flattening f. For WGS-72, use a = 6 378 135 m and f = 1/298.26.*

```haskell
example2 :: IO()
example2 = do
    let frameB =
            LocalFrames.frameB
                (Angle.decimalDegrees 40)
                (Angle.decimalDegrees 20)
                (Angle.decimalDegrees 30)
    let pB = Geodetic.nvectorHeightPos 1 2 3 (Length.metres 400) WGS72
    let delta = LocalFrames.deltaMetres 3000 2000 100

    let pC = LocalFrames.destination pB frameB delta
    -- 53°18'46.839"N,63°29'6.179"E 406.006017m (WGS72)

    putStrLn ("NavLab, Example 2: B and delta to C\n\
              \  pC = " ++ (show pC) ++ "\n")
```

### Example 3: ECEF-vector to geodetic latitude

*Position B is given as an “ECEF-vector” p_EB_E (i.e. a vector from E, the center of the Earth, to B, decomposed in E).
Find the geodetic latitude, longitude and height (latEB, lonEB and hEB), assuming WGS-84 ellipsoid.*

```haskell
example3 :: IO()
example3 = do
    let ecef = Geocentric.metresPos 5733900.0 (-6371000.0) 7008100.000000001 WGS84

    let geod = Positions.toGeodetic ecef
    -- 39°22'43.495"N,48°0'46.035"W 4702.059834295km (WGS84)

    putStrLn ("NavLab, 3: ECEF-vector to geodetic latitude\n\
              \  geodetic pos = " ++ (show geod) ++ "\n")
```

### Example 4: Geodetic latitude to ECEF-vector

*Geodetic latitude, longitude and height are given for position B as latEB, lonEB and hEB, find the ECEF-vector
for this position, p_EB_E.*

```haskell
example4 :: IO()
example4 = do
    let geod = Geodetic.wgs84Pos 1 2 (Length.metres 3)

    let ecef = Positions.toGeocentric geod
    -- Position {gx = 6373.290277218km, gy = 222.560200675km, gz = 110.568827182km, model = WGS84}

    putStrLn ("NavLab, 4: Geodetic latitude to ECEF-vector\n\
              \  geocentric pos = " ++ (show ecef) ++ "\n")
```

### Example 5: Surface distance

*Find the surface distance sAB (i.e. great circle distance) between two positions A and B. The heights of A and B are
ignored, i.e. if they don’t have zero height, we seek the distance between the points that are at the surface of the
Earth, directly above/below A and B. The Euclidean distance (chord length) dAB should also be found.
Use Earth radius 6371e3 m. Compare the results with exact calculations for the WGS-84 ellipsoid.*

```haskell
example5 :: IO()
example5 = do
    let pA = Geodetic.s84Pos 88 0 Length.zero
    let pB = Geodetic.s84Pos 89 (-170) Length.zero

    let distance = GreatCircle.surfaceDistance pA pB
    -- 332.456901835km

    putStrLn ("NavLab, 5: Surface distance\n\
              \  distance = " ++ (show distance) ++ "\n")
```

### Example 6: Interpolated position

*Given the position of B at time t0 and t1, n_EB_E(t0) and n_EB_E(t1).*

*Find an interpolated position at time ti, n_EB_E(ti). All positions are given as n-vectors.*

```haskell
example6 :: IO()
example6 = do
    let pA = Geodetic.s84Pos 89 0 Length.zero
    let pB = Geodetic.s84Pos 89 180 Length.zero
    let f = 0.6

    let interpolated = GreatCircle.interpolated pA pB f
    -- 89°47'59.929"N,180°0'0.000"E 0.0m (S84)

    putStrLn ("NavLab, 6: Interpolated position\n\
              \  interpolated = " ++ (show interpolated) ++ "\n")
```

### Example 7: Mean position

*Three positions A, B, and C are given as n-vectors n_EA_E, n_EB_E, and n_EC_E. Find the mean position, M, given as
n_EM_E. Note that the calculation is independent of the depths of the positions.*

```haskell
example7 :: IO()
example7 = do
    let ps =
            [ Geodetic.s84Pos 90 0 Length.zero
            , Geodetic.s84Pos 60 10 Length.zero
            , Geodetic.s84Pos 50 (-20) Length.zero
            ]

    let mean = GreatCircle.mean ps
    -- Just 67°14'10.150"N,6°55'3.040"W 0.0m (S84)

    putStrLn ("NavLab, Example 7: Mean position\n\
              \  mean = " ++ (show mean) ++ "\n")
```

### Example 8: A and azimuth/distance to B

*We have an initial position A, direction of travel given as an azimuth (bearing) relative to north (clockwise), and finally the distance to travel along a great circle given as sAB. Use Earth radius 6371e3 m to find the destination
point B.*

*In geodesy this is known as “The first geodetic problem” or “The direct geodetic problem” for a sphere, and we see that this is similar to Example 2, but now the delta is given as an azimuth and a great circle distance.
(“The second/inverse geodetic problem” for a sphere is already solved in Examples 1 and 5.)*

```haskell
example8 :: IO()
example8 = do
    let p = Geodetic.s84Pos 80 (-90) Length.zero
    let bearing = Angle.decimalDegrees 200
    let distance = Length.metres 1000

    let dest = GreatCircle.destination p bearing distance
    -- 79°59'29.575"N,90°1'3.714"W 0.0m (S84)

    putStrLn ("NavLab, Example 8: A and azimuth/distance to B\n\
              \  destination = " ++ (show dest) ++ "\n")
```

### Example 9: Intersection of two paths

*Define a path from two given positions (at the surface of a spherical Earth), as the great circle that goes through the two points.*

*Path A is given by A1 and A2, while path B is given by B1 and B2.*

*Find the position C where the two great circles intersect.*

```haskell
example9 :: IO()
example9 = do
    let a1 = Geodetic.s84Pos 51.885 0.235 Length.zero
    let a2 = Geodetic.s84Pos 48.269 13.093 Length.zero
    let b1 = Geodetic.s84Pos 49.008 2.549 Length.zero
    let b2 = Geodetic.s84Pos 56.283 11.304 Length.zero

    let ga = GreatCircle.through a1 a2
    let gb = GreatCircle.through b1 b2
    let intersections = GreatCircle.intersections <$> ga <*> gb
    -- Just (Just (50°54'6.260"N,4°29'39.052"E 0.0m (S84),50°54'6.260"S,175°30'20.947"W 0.0m (S84)))

    let ma = GreatCircle.minorArc a1 a2
    let mb = GreatCircle.minorArc b1 b2
    let intersection = GreatCircle.intersection <$> ma <*> mb
    -- Just (Just 50°54'6.260"N,4°29'39.052"E 0.0m (S84))

    putStrLn ("NavLab, Example 9: Intersection of two paths\n\
              \  great circle intersections = " ++ (show intersections) ++ "\n\
              \  minor arc intersection     = " ++ (show intersection) ++ "\n")
```

### Example 10: Cross track distance

*Path A is given by the two positions A1 and A2 (similar to the previous example).

*Find the cross track distance sxt between the path A (i.e. the great circle through A1 and A2) and the position B
(i.e. the shortest distance at the surface, between the great circle and B).*

```haskell
example10 :: IO()
example10 = do
    let p = Geodetic.s84Pos 1 0.1 Length.zero
    let gc = GreatCircle.through
                 (Geodetic.s84Pos 0 0 Length.zero)
                 (Geodetic.s84Pos 10 0 Length.zero)

    let sxt = fmap (\g -> GreatCircle.crossTrackDistance p g) gc
    -- Just 11.117814411km

    putStrLn ("NavLab, Example 10: Cross track distance\n\
              \  cross track distance = " ++ (show sxt) ++ "\n")
```

## Solutions to the geodesic problems (Vincenty)

### Example 11: Inverse problem

*Given the coordinates of the two points (Φ1, L1) and (Φ2, L2), the inverse problem finds the azimuths α1, α2 and the ellipsoidal distance s.*

```haskell
example11 :: IO()
example11 = do
    let pA = Geodetic.wgs84Pos 88 0 Length.zero
    let pB = Geodetic.wgs84Pos 89 (-170) Length.zero

    let inv = Geodesic.inverse pA pB
    let initialBearing = fmap Geodesic.initialBearing inv
    -- Just (Just 356°40'8.701")

    let finalBearing = fmap Geodesic.finalBearing inv
    -- Just (Just 186°40'19.615")

    let distance = fmap Geodesic.length inv
    -- Just 333.947509469km

    putStrLn ("Geodesic, Example 11: Inverse problem\n\
              \    initial bearing = " ++ (show initialBearing) ++ "\n\
              \    final bearing   = " ++ (show finalBearing) ++ "\n\
              \    distance        = " ++ (show distance) ++ "\n")
```

### Example 12: Direct problem

*Given an initial point (Φ1, L1) and initial azimuth, α1, and a distance, s, along the geodesic the problem is to find the end point (Φ2, L2) and azimuth, α2.*

```haskell
example12 :: IO()
example12 = do
    let p = Geodetic.wgs84Pos 80 (-90) Length.zero
    let bearing = Angle.decimalDegrees 200
    let distance = Length.metres 1000

    let dct = Geodesic.direct p bearing distance
    let destination = fmap Geodesic.endPosition dct
    -- Just 79°59'29.701"N,90°1'3.436"W 0.0m (WGS84)

    let finalBearing = fmap Geodesic.finalBearing dct
    -- Just (Just 199°58'57.528")

    putStrLn ("Geodesic, Example 12: Direct problem\n\
              \    destination   = " ++ (show destination) ++ "\n\
              \    final bearing = " ++ (show finalBearing) ++ "\n")
```

## Solutions to kinematics problems

### Example 13: Closest point of approach

*The Closest Point of Approach (CPA) refers to the positions at which two dynamically moving objects reach their
closest possible distance.*

```haskell
example13 :: IO()
example13 = do
    let ownship = Track
                 (Geodetic.s84Pos 20 (-60) Length.zero)
                 (Angle.decimalDegrees 10)
                 (Speed.knots 15)
    let intruder = Track
                 (Geodetic.s84Pos 34 (-50) (Length.metres 10000))
                 (Angle.decimalDegrees 220)
                 (Speed.knots 300)

    let cpa = Kinematics.cpa ownship intruder
    let timeToCpa = fmap Kinematics.timeToCpa cpa
    -- Just 3H9M56.155S

    let distanceAtCpa = fmap Kinematics.distanceAtCpa cpa
    -- Just 124.231730834km

    let cpaOwnshipPosition = fmap Kinematics.cpaOwnshipPosition cpa
    -- Just 20°46'43.641"N,59°51'11.225"W 0.0m (S84)

    let cpaIntruderPosition = fmap Kinematics.cpaIntruderPosition cpa
    -- Just 21°24'8.523"N,60°50'48.159"W 10000.0m (S84)

    putStrLn ("Kinematics, Example 13: Closest point of approach\n\
              \    time to CPA      = " ++ (show timeToCpa) ++ "\n\
              \    distance at CPA  = " ++ (show distanceAtCpa) ++ "\n\
              \    CPA ownship pos  = " ++ (show cpaOwnshipPosition) ++ "\n\
              \    CPA intruder pos = " ++ (show cpaIntruderPosition) ++ "\n")
```

### Example 14: Speed required to intercept target

*Inputs are the initial latitude and longitude of an interceptor and a target, and the target course and speed.
Also input is the time of the desired intercept. Outputs are the speed required of the interceptor, the course
of the interceptor, the distance travelled to intercept, and the latitude and longitude of the intercept.*

```haskell
example14 :: IO()
example14 = do
    let track = Track
                (Geodetic.s84Pos 34 (-50) Length.zero)
                (Angle.decimalDegrees 220)
                (Speed.knots 600)
    let interceptor = Geodetic.s84Pos 20 (-60) Length.zero
    let interceptTime = Duration.seconds 2700

    let intercept = Kinematics.interceptByTime track interceptor interceptTime
    let distanceToIntercept = fmap Kinematics.distanceToIntercept intercept
    -- Just 1015.302358852km

    let interceptPosition = fmap Kinematics.interceptPosition intercept
    -- Just 28°8'12.046"N,55°27'21.411"W 0.0m (S84)

    let interceptorBearing = fmap Kinematics.interceptorBearing intercept
    -- Just 26°7'11.649"

    let interceptorSpeed = fmap Kinematics.interceptorSpeed intercept
    -- Just 1353.736478km/h

    putStrLn ("Kinematics, Example 14: Speed required to intercept target\n\
              \    distance to intercept = " ++ (show distanceToIntercept) ++ "\n\
              \    intercept position    = " ++ (show interceptPosition) ++ "\n\
              \    interceptor bearing   = " ++ (show interceptorBearing) ++ "\n\
              \    interceptor speed     = " ++ (show interceptorSpeed) ++ "\n")
```

### Example 15: Time required to intercept target

*Inputs are the initial latitude and longitude of an interceptor and a target, and the target course and speed. For a
given interceptor speed, it may or may not be possible to make an intercept.*

*The first algorithm is to compute the minimum interceptor speed required to achieve intercept and the time required to
make such and intercept.*

*The second algorithm queries the user to input an interceptor speed. If the speed is at least that required for intercept
then the time required to intercept is computed.*

```haskell
example15 :: IO()
example15 = do
    let track = Track
                    (Geodetic.s84Pos 34 (-50) Length.zero)
                    (Angle.decimalDegrees 220)
                    (Speed.knots 600)
    let interceptor = Geodetic.s84Pos 20 (-60) Length.zero

    let minIntercept = Kinematics.intercept track interceptor
    let minTimeToIntercept = fmap Kinematics.timeToIntercept minIntercept
    -- Just 1H39M53.831S

    let minDistanceToIntercept = fmap Kinematics.distanceToIntercept minIntercept
    -- Just 162.294627463km

    let minInterceptPosition = fmap Kinematics.interceptPosition minIntercept
    -- Just 20°43'42.305"N,61°20'56.848"W 0.0m (S84)

    let minInterceptorBearing = fmap Kinematics.interceptorBearing minIntercept
    -- Just 300°10'18.053"

    let minInterceptorSpeed = fmap Kinematics.interceptorSpeed minIntercept
    -- Just 97.476999km/h

    putStrLn ("Kinematics, Example 15: Time required to intercept target (min)\n\
              \    time to intercept     = " ++ (show minTimeToIntercept) ++ "\n\
              \    distance to intercept = " ++ (show minDistanceToIntercept) ++ "\n\
              \    intercept position    = " ++ (show minInterceptPosition) ++ "\n\
              \    interceptor bearing   = " ++ (show minInterceptorBearing) ++ "\n\
              \    interceptor speed     = " ++ (show minInterceptorSpeed) ++ "\n")

    let interceptSpeed = Speed.knots 700

    let intercept = Kinematics.interceptBySpeed track interceptor interceptSpeed
    let timeToIntercept = fmap Kinematics.timeToIntercept intercept
    -- Just 0H46M4.692S

    let distanceToIntercept = fmap Kinematics.distanceToIntercept intercept
    -- Just 995.596069189km

    let interceptPosition = fmap Kinematics.interceptPosition intercept
    -- Just 27°59'36.764"N,55°34'43.852"W 0.0m (S84)

    let interceptorBearing = fmap Kinematics.interceptorBearing intercept
    -- Just 25°56'7.484"

    putStrLn ("Kinematics, Example 15: Time required to intercept target\n\
              \    time to intercept     = " ++ (show timeToIntercept) ++ "\n\
              \    distance to intercept = " ++ (show distanceToIntercept) ++ "\n\
              \    intercept position    = " ++ (show interceptPosition) ++ "\n\
              \    interceptor bearing   = " ++ (show interceptorBearing) ++ "\n")
```
***

```haskell
main :: IO()
main = do
    example1
    example2
    example3
    example4
    example5
    example6
    example7
    example8
    example9
    example10
    example11
    example12
    example13
    example14
    example15
```