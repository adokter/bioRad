# Gaussian beam profile as a function of height

Normalized altitudinal pattern of radiated energy as a function of
altitude at a given distance from the radar. `antenna` and `height`
should be given in reference to the same reference plane (e.g. ground
level or sea level).

## Usage

``` r
gaussian_beam_profile(
  height,
  range,
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

- range:

  Numeric. Slant range, i.e. the length of the skywave path between
  target and the radar antenna, in m.

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

numeric.

## Details

Beam profile is calculated using
[beam_height](http://adriaandokter.com/bioRad/dev/reference/beam_height.md)
and
[beam_width](http://adriaandokter.com/bioRad/dev/reference/beam_width.md).
`height` and `antenna` should be given in reference to the same
reference plane (e.g. ground level or sea level)

## See also

Other beam_functions:
[`beam_distance()`](http://adriaandokter.com/bioRad/dev/reference/beam_distance.md),
[`beam_height()`](http://adriaandokter.com/bioRad/dev/reference/beam_height.md),
[`beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile.md),
[`beam_profile_overlap()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile_overlap.md),
[`beam_range()`](http://adriaandokter.com/bioRad/dev/reference/beam_range.md),
[`beam_width()`](http://adriaandokter.com/bioRad/dev/reference/beam_width.md)
