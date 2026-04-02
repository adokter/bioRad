# Get a parameter (`param`) from a scan (`scan`)

Returns the selected parameter (`param`) from a scan (`scan`).

## Usage

``` r
get_param(x, param)
```

## Arguments

- x:

  A `scan` object.

- param:

  Character. A scan parameter, such as `DBZH` or `VRADH`. See
  [`summary.param()`](http://adriaandokter.com/bioRad/dev/reference/summary.param.md)
  for commonly available parameters.

## Value

A `param` object.

## See also

- [`summary.param()`](http://adriaandokter.com/bioRad/dev/reference/summary.param.md)

## Examples

``` r
# Get summary info for a scan (including parameters)
example_scan
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  0.5 deg
#>            dims:  480 bins x 360 rays

# Extract the VRADH scan parameter
param <- get_param(example_scan, "VRADH")

# Get summary info for this parameter
param
#>                Polar scan parameter (class param)
#> 
#>     quantity:  VRADH 
#>         dims:  480 bins x 360 rays
```
