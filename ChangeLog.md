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
