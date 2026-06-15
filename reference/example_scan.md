# Scan (`scan`) example

Example of a
[`scan`](http://adriaandokter.com/bioRad/reference/summary.scan.md)
object with name `example_scan`.

## Usage

``` r
example_scan
```

## Format

An object of class `scan` of dimension 5 x 480 x 360.

## Value

An example object of type `scan` which represents a single scan from a
weather radar.

## See also

- [`summary.scan()`](http://adriaandokter.com/bioRad/reference/summary.scan.md)

## Examples

``` r
# Reload example_scan from package (e.g. in case it was altered)
data(example_scan)

# Get summary info
example_scan
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  0.5 deg
#>            dims:  480 bins x 360 rays
```
