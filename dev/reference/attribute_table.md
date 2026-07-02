# Extract a volume coverage pattern table with all attributes

This function tabulates the attributes of one scan or all scans of a
pvol. Attributes that have a length longer then one are presented as a
list column. By default the function returns a limited set of columns to
keep the output clear. It is important to note that attributes of the
full polar volume can contain additional information on processing that
is not included in the resulting table. This function only tabulates
attributes of the scans.

## Usage

``` r
attribute_table(
  x,
  select = c("how.lowprf", "how.midprf", "how.highprf", "where.elangle", "where.nbins",
    "where.nrays", "where.rscale", "how.NI"),
  ...
)
```

## Arguments

- x:

  Either a pvol or scan for which the table should be created.

- select:

  A character vector which the column names that should be returned when
  NULL all attributes are to be returned

- ...:

  Currently not used

## Value

A data.frame with the attributes of the scan(s)

## See also

Other profile metadata functions:
[`check_night()`](http://adriaandokter.com/bioRad/dev/reference/check_night.md),
[`doy_noy`](http://adriaandokter.com/bioRad/dev/reference/doy_noy.md),
[`rcs()`](http://adriaandokter.com/bioRad/dev/reference/rcs.md),
[`sd_vvp_threshold()`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold.md)

## Examples

``` r
data(example_scan)
attribute_table(example_scan)
#>    how.NI how.highprf how.lowprf where.elangle where.nbins where.nrays
#>  24.06897         600        450           0.5         480         360
#>  where.rscale                          param
#>           500 DBZH, VRADH, RHOHV, ZDR, PHIDP

pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
example_pvol <- read_pvolfile(pvolfile)
attribute_table(example_pvol)
#>     how.NI how.highprf how.lowprf where.elangle where.nbins where.nrays
#>   24.06897         600        450           0.5         480         360
#> 1 24.06897         600        450           1.5         480         360
#> 2 24.06897         600        450           2.5         480         360
#>   where.rscale                          param
#>            500 DBZH, VRADH, RHOHV, ZDR, PHIDP
#> 1          500 DBZH, VRADH, RHOHV, ZDR, PHIDP
#> 2          500 DBZH, VRADH, RHOHV, ZDR, PHIDP
```
