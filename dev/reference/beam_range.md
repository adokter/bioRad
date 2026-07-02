# Calculate radar beam range

Calculates the range (i.e. slant range) given a distance measured along
the earth's surface (i.e. down range) and beam elevation.

## Usage

``` r
beam_range(distance, elev, k = 4/3, lat = 35, re = 6378, rp = 6357)
```

## Arguments

- distance:

  Numeric. Distance from the radar as measured along sea level (down
  range), in m.

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

Beam range (slant range), in m.

## Details

depends on
[beam_height](http://adriaandokter.com/bioRad/dev/reference/beam_height.md)
to calculate beam height.

## See also

Other beam functions:
[`beam_distance()`](http://adriaandokter.com/bioRad/dev/reference/beam_distance.md),
[`beam_height()`](http://adriaandokter.com/bioRad/dev/reference/beam_height.md),
[`beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile.md),
[`beam_profile_overlap()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile_overlap.md),
[`beam_width()`](http://adriaandokter.com/bioRad/dev/reference/beam_width.md)

## Examples

``` r
# Slant range of the 5 degree elevation beam at a down range of 100 km
beam_range(100000, 5)
#> [1] 100508.1
```
