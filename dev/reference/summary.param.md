# Inspect a parameter (`param`)

R base functions for inspecting a parameter (`param`) object.

## Usage

``` r
# S3 method for class 'param'
summary(object, ...)

is.param(x)
```

## Arguments

- object:

  A `param` object.

- ...:

  Additional arguments affecting the summary produced.

- x:

  A `param` object.

## Value

For `is.param()`: `TRUE` for an object of class `param`, otherwise
`FALSE`.

## Details

A parameter is a quantity/variable measured by the radar during a scan
(or sweep). These are organized along radar range (bins) and azimuth
(rays). Scan parameters are named according to the OPERA data
information model (ODIM), see Table 16 in the [ODIM
specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf).

Commonly available parameters are:

- `DBZH`, `DBZ`: (Logged) reflectivity factor in dBZ.

- `TH`, `T`: (Logged) uncorrected reflectivity factor in dBZ.

- `VRADH`, `VRAD`: Radial velocity in m/s. Radial velocities towards the
  radar are negative, while radial velocities away from the radar are
  positive.

- `RHOHV`: Correlation coefficient (unitless). Correlation between the
  vertically and horizontally polarized reflectivity factor.

- `PHIDP`: Differential phase in degrees.

- `ZDR`: (Logged) differential reflectivity in dB.

## See also

- [`get_param()`](http://adriaandokter.com/bioRad/dev/reference/get_param.md)

## Examples

``` r
# Extract the DBZH parameter from a scan
param <- get_param(example_scan, "DBZH")

# Check if it is an object of class param
is.param(param)
#> [1] TRUE

# Get summary info for this parameter
param # Same as summary(param) or print(param)
#>                Polar scan parameter (class param)
#> 
#>     quantity:  DBZH 
#>         dims:  480 bins x 360 rays
```
