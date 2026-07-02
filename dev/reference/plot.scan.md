# Plot a scan (`scan`) in polar coordinates

Plots a scan (`scan`) in polar coordinates. To plot in Cartesian
coordinates, see
[`project_as_ppi()`](http://adriaandokter.com/bioRad/dev/reference/project_as_ppi.md).

## Usage

``` r
# S3 method for class 'scan'
plot(
  x,
  param,
  xlim = c(0, 1e+05),
  ylim = c(0, 360),
  zlim = c(-20, 20),
  na.value = "transparent",
  ...
)
```

## Arguments

- x:

  A `scan` object.

- param:

  Character. Scan parameter to plot, e.g. `DBZH` or `VRADH`. See
  [`summary.param()`](http://adriaandokter.com/bioRad/dev/reference/summary.param.md)
  for commonly available parameters.

- xlim:

  Numeric vector of length 2. Range of x values (range, distance to
  radar) to plot.

- ylim:

  Numeric vector of length 2. Range of y values (azimuth) to plot.

- zlim:

  Numeric vector of length 2. The range of parameter values to plot.
  Defaults to parameter specific limits for plotting, not full range of
  data.

- na.value:

  Character.
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  parameter to set the color of `NA` values.

- ...:

  Arguments passed to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Value

No return value, side effect is a plot.

## Details

Available scan parameters for plotting can by printed to screen by
`summary(x)`. Commonly available parameters are:

- `DBZH`, `DBZ`: (Logged) reflectivity factor (dBZ)

- `TH`, `T`: (Logged) uncorrected reflectivity factor (dBZ)

- `VRADH`, `VRAD`: Radial velocity (m/s). Radial velocities towards the
  radar are negative, while radial velocities away from the radar are
  positive

- `RHOHV`: Correlation coefficient (unitless). Correlation between
  vertically polarized and horizontally polarized reflectivity factor

- `PHIDP`: Differential phase (degrees)

- `ZDR`: (Logged) differential reflectivity (dB) The scan parameters are
  named according to the OPERA data information model (ODIM), see Table
  16 in the [ODIM
  specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf).

## See also

Other scan functions:
[`example_scan`](http://adriaandokter.com/bioRad/dev/reference/example_scan.md),
[`get_elevation_angles()`](http://adriaandokter.com/bioRad/dev/reference/get_elevation_angles.md),
[`get_scan()`](http://adriaandokter.com/bioRad/dev/reference/get_scan.md),
[`summary.scan()`](http://adriaandokter.com/bioRad/dev/reference/summary.scan.md),
[`tidyverse`](http://adriaandokter.com/bioRad/dev/reference/tidyverse.md)

## Examples

``` r
# Plot reflectivity
plot(example_scan, param = "DBZH")


# \donttest{
# Change the range of reflectivities to plot, from -10 to 10 dBZ
plot(example_scan, param = "DBZH", zlim = c(-10, 10))


# Change the scale name, change the color palette to Viridis colors
plot(example_scan, param = "DBZH", zlim = c(-10, 10)) +
  viridis::scale_fill_viridis(name = "dBZ")
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.

# }
```
