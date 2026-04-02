# Calculate radar beam width

Calculates the width of a radar beam as a function of range and beam
angle.

## Usage

``` r
beam_width(range, beam_angle = 1, path = "two_way")
```

## Arguments

- range:

  Numeric. Range, i.e. distance from the radar antenna, in m.

- beam_angle:

  Numeric. Beam opening angle in degrees, typically speciefied as the
  angle between the half-power (-3 dB) points of the main lobe for the
  one-way antenna pattern.

- path:

  Character. One of `two_way` (default) or `one_way` for specifying the
  effective beam width for the radar's antenna pattern as it transmits a
  signal (`one_way`), or as it transmits and receives a signal
  (`two_way`).

## Value

numeric. Beam width in m, typically the full width at half maximum
(FWHM).

## Details

The two-way beam is effectively narrower than the one-way beam because
the power distribution is squared in the two-way path (transmit and
receive). Using the normal approximation for the beam power profile,
this means the two-way beam width equals the one-way beam width divided
by `sqrt(2)`.

## See also

Other beam_functions:
[`beam_distance()`](http://adriaandokter.com/bioRad/dev/reference/beam_distance.md),
[`beam_height()`](http://adriaandokter.com/bioRad/dev/reference/beam_height.md),
[`beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile.md),
[`beam_profile_overlap()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile_overlap.md),
[`beam_range()`](http://adriaandokter.com/bioRad/dev/reference/beam_range.md),
[`gaussian_beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/gaussian_beam_profile.md)

## Examples

``` r
#' # Beam width in meters at 10 km range:
beam_width(10000)
#> [1] 123.4103

# Define ranges from 0 to 1000000 m (100 km), in steps of 100 m:
range <- seq(0, 100000, 100)

# Plot the two-way beam width as a function of range:
plot(range, beam_width(range), ylab = "two-way beam width [m]", xlab = "range [m]")


#' # Plot the one-way beam width as a function of range:
plot(range, beam_width(range), ylab = "one-way beam width [m]", xlab = "range [m]")
```
