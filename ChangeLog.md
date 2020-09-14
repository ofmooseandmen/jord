### 2.0.0.0

- API change - all modules should be imported as qualified
- API change - Position has been split between Geodetic & Geocentric
- API change - positions conversion/transformation are provided by Positions module
- API change - GreatCicle: interpolate -> interpolated, isInsideSurface -> enclosedBy
- API change - Geodesic data type provides initialBearing, finalBearing, length and functions are removed
- API change - LocalFrames -> Local
- API change - provide separate data type for horizontal positions and positions with height
- New API    - GreatCircle: turn & side
- New API    - Polygon: simple, circle, arc, contains & triangulate
- New API    - Triangle: contains, circumcentre & centroid
- Bug Fix    - GreatCircle.intersection correctly handles minor arcs crossing the equator
- Bug Fix    - All Geodetic positions built from a n-vector have the correct angle resolution
- Bump stack resolver to lts-16.11
- Examples in readme are compile to make sure they are valid
- Use github actions for CI

### 1.0.0.1

- Fixed typo in doc
- Bumped stack resolver to latest

### 1.0.0.0

- New API (does not allow mixing position in different coordinate systems)
- Exact solution for both direct and inverse geodetic problems (Vincenty)
- Conversion between different coordinate systems

### 0.6.0.0

- Fixed Ellipsoid: constructor expected inverseFlattening and not flattening

### 0.5.0.0

- Added Benchmarks
- Added GreatArc
- Added GreatArc from tuple of positions
- Added GreatArc from GreatCircle
- Added GreatArc from Track and Duration
- Added alongTrackDistance
- Added GreatArcs intersection

### 0.4.2.0

- Fixed intercept
- jord-exe renamed jord-repl

### 0.4.1.0

- Fixed interceptBySpeed
- Nautical miles symbol is "nm"
- REPL: intercept for intercept, interceptBySpeed and interceptByTime
- REPL: show length and speed in user selected unit

### 0.4.0.0

- Added ECEF, frames and delta to REPL
- Added Speed
- Added Duration
- Added Kinematics: course, position, CPA and intercept

### 0.3.1.0

- Added ECEF position
- Added Frames (Body, Local, North East Down)
- Added delta and target from position(s), frame and earth model
- Added earth models (WGS84, WGS72, GRS80 and derived spherical models)
- Builds against LTS 12.2 (GHC 8.4.3) and LTS 11.18 (GHC 8.2.2)

### 0.2.0.0

- GeoPos -> LatLong
- Split Position from GreatCircle
- require base >= 4.9

### 0.1.0.0

- Initial version
