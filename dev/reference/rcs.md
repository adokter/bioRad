# Get radar cross section

Returns the currently assumed radar cross section of an object in cm^2.

## Usage

``` r
rcs(x)

# S3 method for class 'vp'
rcs(x)

# S3 method for class 'list'
rcs(x)

# S3 method for class 'vpts'
rcs(x)

# S3 method for class 'vpi'
rcs(x)
```

## Arguments

- x:

  A `vp`, list of `vp`, `vpts` or `vpi` object.

## Value

The radar cross section in cm^2.

## See also

Other profile metadata functions:
[`attribute_table()`](http://adriaandokter.com/bioRad/dev/reference/attribute_table.md),
[`check_night()`](http://adriaandokter.com/bioRad/dev/reference/check_night.md),
[`doy_noy`](http://adriaandokter.com/bioRad/dev/reference/doy_noy.md),
[`sd_vvp_threshold()`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold.md)

## Examples

``` r
# Get the radar cross section for a vp
rcs(example_vp)
#> [1] 11

# Get the radar cross section for a vpts
rcs(example_vpts)
#> [1] 11

# Get the radar cross section for a vpi
vpi <- integrate_profile(example_vpts)
rcs(vpi)
#> [1] 11
```
