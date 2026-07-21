# Map a plan position indicator (`ppi`) on a map

Plots a plan position indicator (`ppi`) on a base layer

## Usage

``` r
map(x, ...)

# S3 method for class 'ppi'
map(
  x,
  map = "cartolight",
  param,
  alpha = 0.7,
  xlim,
  ylim,
  zlim = c(-20, 20),
  ratio,
  radar_size = 3,
  radar_color = "#202020",
  n_color = 1000,
  palette = NA,
  zoomin = -2,
  cachedir = tools::R_user_dir("bioRad"),
  ...
)
```

## Arguments

- x:

  A `ppi` object.

- ...:

  Arguments passed to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- map:

  Basemap to use, one of
  [`rosm::osm.types()`](https://rdrr.io/pkg/rosm/man/deprecated.html)

- param:

  Character. Scan parameter to plot, e.g. `DBZH` or `VRADH`. See
  [`summary.param()`](http://adriaandokter.com/bioRad/dev/reference/summary.param.md)
  for commonly available parameters.

- alpha:

  Numeric. Transparency of the data, value between 0 and 1.

- xlim:

  Numeric vector of length 2. Range of x values (degrees longitude) to
  plot.

- ylim:

  Numeric vector of length 2. Range of y values (degrees latitude) to
  plot.

- zlim:

  Numeric vector of length 2. The range of values to plot.

- ratio:

  Numeric. Aspect ratio between x and y scale, by default
  \\1/cos(latitude radar \* pi/180)\\.

- radar_size:

  Numeric. Size of the symbol indicating the radar position. Use `NULL`
  if you do not want to plot the radar point. This might be useful the
  radar is outside the range of data.

- radar_color:

  Character. Color of the symbol indicating the radar position.

- n_color:

  Numeric. Number of colors (\>=1) to use in the palette.

- palette:

  Character vector. Hexadecimal color values defining the plot color
  scale, e.g. output from
  [`viridisLite::viridis()`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html).

- zoomin:

  Numeric. Maps to
  [`ggspatial::annotation_map_tile()`](https://paleolimbot.github.io/ggspatial/reference/annotation_map_tile.html)

- cachedir:

  Character. Maps to
  [`ggspatial::annotation_map_tile()`](https://paleolimbot.github.io/ggspatial/reference/annotation_map_tile.html),
  defaults to `tools::R_user_dir("bioRad")`

## Value

A ggplot object

## Details

Available scan parameters for mapping can by printed to screen by
`summary(x)`. Commonly available parameters are:

- `DBZH`, `DBZ`: (Logged) reflectivity factor (dBZ)

- `TH`, `T`: (Logged) uncorrected reflectivity factor (dBZ)

- `VRADH`, `VRAD`: Radial velocity (m/s). Radial velocities towards the
  radar are negative, while radial velocities away from the radar are
  positive

- `RHOHV`: Correlation coefficient (unitless) Correlation between
  vertically polarized and horizontally polarized reflectivity factor

- `PHIDP`: Differential phase (degrees)

- `ZDR`: (Logged) differential reflectivity (dB) The scan parameters are
  named according to the OPERA data information model (ODIM), see Table
  16 in the [ODIM
  specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf).

## Methods (by class)

- `map(ppi)`: Plot a `ppi` object on a map.

## See also

Other ppi functions:
[`[.ppi()`](http://adriaandokter.com/bioRad/dev/reference/sub-.ppi.md),
[`plot.ppi()`](http://adriaandokter.com/bioRad/dev/reference/plot.ppi.md),
[`summary.ppi()`](http://adriaandokter.com/bioRad/dev/reference/summary.ppi.md)

## Examples

``` r
# Project a scan as a ppi
ppi <- project_as_ppi(example_scan)
# \donttest{
if (all(sapply(c("ggspatial","prettymapr", "rosm"), requireNamespace, quietly = TRUE))) {
# Choose a basemap
basemap <- rosm::osm.types()[1]

# Map the radial velocity of the ppi onto the basemap
map(ppi, map = basemap, param = "VRADH")

# Extend the plotting range of velocities, from -50 to 50 m/s
map(ppi, map = basemap, param = "VRADH", zlim = c(-50, 50))

# Map the reflectivity
map(ppi, map = basemap, param = "DBZH")

# Change the color palette to Viridis colors
map(ppi, map = basemap, param = "DBZH", palette = viridis::viridis(100), zlim=c(-10,10))

# Give the data more transparency
map(ppi, map = basemap, param = "DBZH", alpha = 0.3)

# Change the appearance of the symbol indicating the radar location
map(ppi, map = basemap, radar_size = 5, radar_color = "blue")

# Crop the map
map(ppi, map = basemap, xlim = c(12.4, 13.2), ylim = c(56, 56.5))
}
#> Zoom: 9
#> Fetching 4 missing tiles
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> ...complete!

# }
```
