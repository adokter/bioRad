# Calculate radar beam distance

Calculates the distance as measured over the earth's surface (the down
range) for a given beam elevation and slant range.

## Usage

``` r
beam_distance(range, elev, k = 4/3, lat = 35, re = 6378, rp = 6357)
```

## Arguments

- range:

  Numeric. Slant range, i.e. the length of the skywave path between
  target and the radar antenna, in m.

- elev:

  Numeric. Beam elevation, in degrees.

- k:

  Numeric. Standard refraction coefficient.

- lat:

  Numeric. Geodetic latitude of the radar, in degrees.

- re:

  Numeric. Earth equatorial radius, in km.

- rp:

  Numeric. Earth polar radius, in km.

## Value

Beam distance (down range), in m.

## Details

depends on
[beam_height](http://adriaandokter.com/bioRad/dev/reference/beam_height.md)
to calculate beam height.

## See also

- [`beam_height()`](http://adriaandokter.com/bioRad/dev/reference/beam_height.md)

Other beam_functions:
[`beam_height()`](http://adriaandokter.com/bioRad/dev/reference/beam_height.md),
[`beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile.md),
[`beam_profile_overlap()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile_overlap.md),
[`beam_range()`](http://adriaandokter.com/bioRad/dev/reference/beam_range.md),
[`beam_width()`](http://adriaandokter.com/bioRad/dev/reference/beam_width.md),
[`gaussian_beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/gaussian_beam_profile.md)

## Examples

``` r
# Down range of the 5 degree elevation beam at a slant range of 100 km:
beam_distance(100000, 5)
#> [1] 99495.13
```
