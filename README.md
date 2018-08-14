# Jord - Geographical Position Calculations

[![travis build status](https://img.shields.io/travis/ofmooseandmen/jord/master.svg?label=travis+build)](https://travis-ci.org/ofmooseandmen/jord)
[![Hackage](https://img.shields.io/hackage/v/jord.svg)](http://hackage.haskell.org/package/jord)
[![license](https://img.shields.io/badge/license-BSD3-lightgray.svg)](https://opensource.org/licenses/BSD-3-Clause)

> __Jord__ [_Swedish_] is __Earth__ [_English_]

## What is this?

Jord is a [Haskell](https://www.haskell.org) library that implements various geographical position calculations using the algorithms described in [Gade, K. (2010). A Non-singular Horizontal Position Representation](http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf).

- Transformation between ECEF (earth-centred, earth-fixed), Latitude/Longitude and N-Vector positions for spherical and ellipsoidal earth model
- Transformation between Latitude/Longitude and N-Vector positions
- Local, Body and North, East, Down Frames: delta between positions, target position from reference position and delta
- surface distance, initial & final bearing, interpolated position, great circle intersections, cross track distance, ...

## How do I build it?

If you have [Stack](https://docs.haskellstack.org/en/stable/README/),
then:
```sh
$ stack build --test
```

## How do I use it?

[See documentation on Hackage](http://hackage.haskell.org/package/jord/docs/Data-Geo-Jord.html)

```haskell
import Data.Geo.Jord

-- Delta between positions in frameL
let p1 = decimalLatLongHeight 1 2 (metres (-3))
let p2 = decimalLatLongHeight 4 5 (metres (-6))
let w = decimalDegrees 5 -- wander azimuth
deltaBetween p1 p2 (frameL w) wgs84 -- deltaMetres 359490.579 302818.523 17404.272

-- destination position from 531914N0014347W having travelled 500Nm on a heading of 96.0217°
-- using mean earth radius derived from the WG84 ellipsoid
destination (readLatLong "531914N0014347W") (decimalDegrees 96.0217) (nauticalMiles 500) r84

-- surface distance between 54°N,154°E and its antipodal position
-- using mean earth radius derived from the WG84 ellipsoid
let p = decimalLatLong 54 154
surfaceDistance p (antipode p) r84
```

Jord comes with a REPL (built with [haskeline](https://github.com/judah/haskeline)):

```sh
$ jord-exe
jord> finalBearing (destination (antipode 54°N,154°E) 54° 1000m) 54°N,154°E
jord> angle: 126°0'0.0" (126.0)
jord> f = frameB 10d 20d 30d
jord> Body (vehicle) frame: yaw=10°0'0.000", pitch=20°0'0.000", roll=30°0'0.000"
jord> d = delta 3000 2000 100
jord> Delta: x=3000.0m, y=2000.0m, z=100.0m
jord> p0 = geo 49.66618 3.45063 0
jord> latitude, longitude, height:
  49°39'58.248"N,3°27'2.268"E
  49.66618, 3.45063
  height: 0.0m
jord> target p0 f d wgs84
jord> latitude, longitude, height:
  49°41'30.486"N,3°28'52.561"E
  49.69180166666667, 3.4812669444444446
  height: 6.007m
jord>  
```
