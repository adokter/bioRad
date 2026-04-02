# Calculate a new scan parameter

Calculate a new parameter (`param`) for a scan (`scan`) or polar volume
(`pvol`)

## Usage

``` r
calculate_param(x, ...)

# S3 method for class 'pvol'
calculate_param(x, ...)

# S3 method for class 'ppi'
calculate_param(x, ...)

# S3 method for class 'scan'
calculate_param(x, ...)
```

## Arguments

- x:

  A `pvol` or `scan` object.

- ...:

  An expression defining the new scan parameter in terms of existing
  scan parameters.

## Value

An object of the same class as `x`, either a `pvol` or `scan`.

## Details

Calculates a new scan parameter (`param`) from a combination of existing
scan parameters. Useful for calculating quantities that are defined in
terms of other basic radar moments, like linear reflectivity eta,
depolarization ratio (Kilambi et al. 2018), or for applying clutter
corrections (`CCORH`) to uncorrected reflectivity moments (`TH`) as
`TH + CCORH`.

For the expression to work it is important that the operation can be
vectorized. For example the `base` `ifelse` function is not vectorized,
in these cases alternatives can be used (e.g.
[`dplyr::if_else`](https://dplyr.tidyverse.org/reference/if_else.html)).

Also note that some functions do not operate on a `matrix` or `param`
object. One example is the
[`dplyr::if_else`](https://dplyr.tidyverse.org/reference/if_else.html)
function. A workaround is calling the
[`c()`](https://rdrr.io/r/base/c.html) function on a parameter to
convert it to a vector (e.g. `c(DBZH)`, see examples).

## Methods (by class)

- `calculate_param(pvol)`: Calculate a new parameter (`param`) for all
  scans in a polar volume (`pvol`).

- `calculate_param(ppi)`: Calculate a new parameter (`param`) for a plan
  position indicator (`ppi`).

- `calculate_param(scan)`: Calculate a new parameter (`param`) for a
  scan (`scan`).

## References

- Kilambi A, Fabry F, Meunier V (2018) A simple and effective method for
  separating meteorological from nonmeteorological targets using
  dual-polarization data. Journal of Atmospheric and Oceanic Technology
  35, pp. 1415–1424.
  [doi:10.1175/JTECH-D-17-0175.1](https://doi.org/10.1175/JTECH-D-17-0175.1)

## See also

- [`get_param()`](http://adriaandokter.com/bioRad/dev/reference/get_param.md)

## Examples

``` r
# Locate and read the polar volume example file
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

# Calculate linear reflectivity ETA from reflectivity factor DBZH
radar_wavelength <- pvol$attributes$how$wavelength
pvol <- calculate_param(pvol, ETA = dbz_to_eta(DBZH, radar_wavelength))
#> Error in eval(nn <- (calc[[i]]), x$params): object 'radar_wavelength' not found

# Add depolarization ratio (DR) as a scan parameter (see Kilambi 2018)
pvol <- calculate_param(pvol, DR = 10 * log10((ZDR + 1 - 2 * ZDR^0.5 * RHOHV) /
  (ZDR + 1 + 2 * ZDR^0.5 * RHOHV)))

# The function also works on scan and ppi objects
calculate_param(example_scan, DR = 10 * log10((ZDR + 1 - 2 * ZDR^0.5 * RHOHV) /
  (ZDR + 1 + 2 * ZDR^0.5 * RHOHV)))
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP DR 
#> elevation angle:  0.5 deg
#>            dims:  480 bins x 360 rays

# set all reflectivity  values to NA when correlation coefficient > 0.95
# (indicating precipitation)
if (require(dplyr, quietly = TRUE)) {
  calculate_param(pvol,
    DBZH=if_else(c(RHOHV)>.95, NA, c(DBZH)) )
}
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#>                Polar volume (class pvol)
#> 
#>      # scans:  3 
#>        radar:  seang 
#>       source:  WMO:02606,RAD:SE50,PLC:Angelholm,NOD:seang,ORG:82,CTY:643,CMT:Swedish radar 
#> nominal time:  2015-10-18 18:00:00 
#> 

# it also works for ppis
ppi <- project_as_ppi(example_scan)
calculate_param(ppi, exp(DBZH))
#>                Plan position indicator (class ppi)
#> 
#>   parameters:  DBZH VRADH RHOHV ZDR PHIDP exp(DBZH) 
#>         dims:  201 x 201 pixels
#> 
```
