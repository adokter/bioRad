# Time series of vertical profiles (`vpts`) example

Example of a
[`vpts`](http://adriaandokter.com/bioRad/dev/reference/summary.vpts.md)
object with name `example_vpts`.

## Usage

``` r
example_vpts
```

## Format

An object of class `vpts` of dimension 1934 x 25 x 15.

## Value

An example object of type `vpts` which represents a time series of
vertical profiles.

## See also

Other vpts functions:
[`[.vpts()`](http://adriaandokter.com/bioRad/dev/reference/sub-.vpts.md),
[`plot.vpts()`](http://adriaandokter.com/bioRad/dev/reference/plot.vpts.md),
[`summary.vpts()`](http://adriaandokter.com/bioRad/dev/reference/summary.vpts.md)

## Examples

``` r
# Reload example_vpts from package (e.g. in case it was altered)
data(example_vpts)

# Get summary info
example_vpts
#>                    Irregular time series of vertical profiles (class vpts)
#> 
#>            radar:  KBGM 
#>       # profiles:  1934 
#> time range (UTC):  2016-09-01 00:02:00 - 2016-09-10 11:56:00 
#>    time step (s):  min: 180     max:  16320 
```
