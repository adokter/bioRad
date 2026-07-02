# Inspect a polar volume (`pvol`)

R base functions for inspecting a polar volume (`pvol`) object.

## Usage

``` r
# S3 method for class 'pvol'
summary(object, ...)

# S3 method for class 'pvol'
print(x, digits = max(3L, getOption("digits") - 3L), ...)

is.pvol(x)

# S3 method for class 'pvol'
dim(x)
```

## Arguments

- object:

  A `pvol` object.

- ...:

  Additional arguments affecting the summary produced.

- x:

  A `pvol` object.

- digits:

  minimal number of *significant* digits, see
  [`print.default`](https://rdrr.io/r/base/print.default.html).

## Value

For `is.pvol()`: `TRUE` for an object of class `pvol`, otherwise
`FALSE`.

For `dim.pvol()`: number of scans (`scan`) in a polar volume (`pvol`).

## Details

A polar volume consists of a number of scans (or sweeps) made by the
radar at different elevation angles. A polar volume (`pvol`) object is a
list containing:

- `radar`: Radar identifier.

- `datetime`: Nominal time of the volume in UTC.

- `scans`: List of scans (`scan`) at different elevation angles.

- `attributes`: List of the volume's `what`, `where` and `how`
  attributes.

- `geo`: List of the volume's geographic properties:

  - `lat`: Latitude of the radar in decimal degrees.

  - `lon`: Longitude of the radar in decimal degrees.

  - `height`: Height of the radar antenna in meters above sea level.

## See also

Other pvol functions:
[`get_elevation_angles()`](http://adriaandokter.com/bioRad/dev/reference/get_elevation_angles.md),
[`tidyverse`](http://adriaandokter.com/bioRad/dev/reference/tidyverse.md)

## Examples

``` r
# Locate and read the polar volume example file
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

# Check if it is an object of class pvol
is.pvol(pvol)
#> [1] TRUE

# Get summary info
pvol # Same as summary(pvol) or print(pvol)
#>                Polar volume (class pvol)
#> 
#>      # scans:  3 
#>        radar:  seang 
#>       source:  WMO:02606,RAD:SE50,PLC:Angelholm,NOD:seang,ORG:82,CTY:643,CMT:Swedish radar 
#> nominal time:  2015-10-18 18:00:00 
#> 

# Get dimensions
dim(pvol)
#> [1] 3

# Get summary info for the scans in the polar volume
pvol$scans
#> [[1]]
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  0.5 deg
#>            dims:  480 bins x 360 rays
#> 
#> [[2]]
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  1.5 deg
#>            dims:  480 bins x 360 rays
#> 
#> [[3]]
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  2.5 deg
#>            dims:  480 bins x 360 rays
#> 
```
