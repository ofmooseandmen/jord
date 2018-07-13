# ☷ Jord

[![travis build status](https://img.shields.io/travis/ofmooseandmen/jord/master.svg?label=travis+build)](https://travis-ci.org/ofmooseandmen/jord)
[![license](https://img.shields.io/badge/license-BSD3-lightgray.svg)](https://opensource.org/licenses/BSD-3-Clause)

> __Jord__ [_Swedish_] is __Earth__ [_English_]

Geographic position calculations on great circles.

## What is this?

Jord is a [Haskell](https://www.haskell.org) library that implements various geographical position calculations on great circles using the algorithms described in [Gade, K. (2010). A Non-singular Horizontal Position Representation](http://www.navlab.net/Publications/A_Nonsingular_Horizontal_Position_Representation.pdf).

## How do I build it?

If you have [Stack](https://docs.haskellstack.org/en/stable/README/),
then:

    $ stack build --test

## How do I use it?

```haskell
import Data.Geo.Jord

-- destination position from 531914N0014347W having travelled 500Nm on a heading of 96.0217°
destination' (readGeoPos "531914N0014347W") (decimalDegrees 96.0217) (nauticalMiles 500)

-- distance between 54°N,154°E and its antipodal position
let p = geoPos (decimalDegrees 54) (decimalDegrees 154)
distance' p (antipode p)
```

Jord comes with a REPL (built with [haskeline](https://github.com/judah/haskeline)) that provides access to all position calculations:

```sh
$ jord-exe
☷ finalBearing (destination (antipode 54°N,154°E) 54° 1000m) 54°N,154°E
☷ angle: 126°0'0.0"
```
