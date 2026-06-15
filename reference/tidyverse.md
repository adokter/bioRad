# Tidyverse methods for bioRad objects

Tidyverse methods for bioRad objects, currently only select is
implemented. When `dplyr` or `tidyverse` is loaded the `select` method
can be used to select parameters.

## Usage

``` r
# S3 method for class 'scan'
select(.data, ...)
```

## Arguments

- .data:

  data object of class `scan` or `pvol`

- ...:

  other arguments

## Examples

``` r
if (require(dplyr, quietly = TRUE)) {
# locate example volume file:
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")

# load the file:
example_pvol <- read_pvolfile(pvolfile)
pvol_selected<-select(example_pvol, DBZH, ZDR) 
# Only selected parameters are retained in the pvol
get_scan(pvol_selected, 1.5)
# Also a series of parameters in a scan can be selected
select(get_scan(example_pvol, 2.5), VRADH:ZDR)
}
#>                   Polar scan (class scan)
#> 
#>      parameters:  VRADH RHOHV ZDR 
#> elevation angle:  2.5 deg
#>            dims:  480 bins x 360 rays
```
