# Calculate vertical radiation profile

Calculates for a set of beam elevations (`elev`) the altitudinal
normalized distribution of radiated energy by those beams. Is a function
of altitude (`height`) at a given distance (`distance`) from the radar,
assuming the beams are emitted at antenna level

## Usage

``` r
beam_profile(
  height,
  distance,
  elev,
  antenna = 0,
  beam_angle = 1,
  k = 4/3,
  lat = 35,
  re = 6378,
  rp = 6357,
  path = "two_way"
)
```

## Arguments

- height:

  Numeric. Height in m.

- distance:

  Numeric. Distance from the radar as measured along sea level (down
  range), in m.

- elev:

  Numeric. Beam elevation, in degrees.

- antenna:

  Numeric. Height of the centre of the radar antenna, in m.

- beam_angle:

  Numeric. Beam opening angle in degrees, typically speciefied as the
  angle between the half-power (-3 dB) points of the main lobe for the
  one-way antenna pattern.

- k:

  Numeric. Standard refraction coefficient.

- lat:

  Numeric. Geodetic latitude of the radar, in degrees.

- re:

  Numeric. Earth equatorial radius, in km.

- rp:

  Numeric. Earth polar radius, in km.

- path:

  Character. One of `two_way` (default) or `one_way` for specifying the
  effective beam width for the radar's antenna pattern as it transmits a
  signal (`one_way`), or as it transmits and receives a signal
  (`two_way`).

## Value

Numeric vector. Normalized radiated energy at each of the specified
heights.

## Details

Beam profile is calculated using
[beam_height](http://adriaandokter.com/bioRad/dev/reference/beam_height.md)
and
[beam_width](http://adriaandokter.com/bioRad/dev/reference/beam_width.md).
Returns a beam profile as a function of height relative to ground level.

Returns the normalized altitudinal pattern of radiated energy as a
function of altitude at a given distance from the radar, assuming the
beams are emitted at antenna level.

## See also

Other beam functions:
[`beam_distance()`](http://adriaandokter.com/bioRad/dev/reference/beam_distance.md),
[`beam_height()`](http://adriaandokter.com/bioRad/dev/reference/beam_height.md),
[`beam_profile_overlap()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile_overlap.md),
[`beam_range()`](http://adriaandokter.com/bioRad/dev/reference/beam_range.md),
[`beam_width()`](http://adriaandokter.com/bioRad/dev/reference/beam_width.md)

## Examples

``` r
# Plot the beam profile, for a 0.5 degree elevation beam at 50 km distance
# from the radar:
plot(beam_profile(height = 0:4000, 50000, 0.5), 0:4000,
  xlab = "normalized radiated energy",
  ylab = "height [m]", main = "beam elevation: 0.5 deg, distance=50km"
)


# Plot the beam profile, for a 2 degree elevation beam at 50 km distance
# from the radar:
plot(beam_profile(height = 0:4000, 50000, 2), 0:4000,
  xlab = "normalized radiated energy",
  ylab = "height [m]", main = "beam elevation: 2 deg, distance=50km"
)


# Plot the combined beam profile for a 0.5 and 2.0 degree elevation beam
# at 50 km distance from the radar:
plot(beam_profile(height = 0:4000, 50000, c(0.5, 2)), 0:4000,
  xlab = "normalized radiated energy",
  ylab = "height [m]", main = "beam elevations: 0.5,2 deg, distance=50km"
)
```
