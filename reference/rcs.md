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

- [`rcs()<-`](http://adriaandokter.com/bioRad/reference/rcs-set.md) for
  setting the radar cross section of an object.

- [`sd_vvp_threshold()`](http://adriaandokter.com/bioRad/reference/sd_vvp_threshold.md)

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
