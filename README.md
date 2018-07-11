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
```

Jord comes with a REPL (still lacking history and completion at the time of writing) that provides access to all position calculations:

    $ jord-exe
    ☷ finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readGeoPos 54°N,154°E)
    ☷ angle: 126°0'0.0"
