# Get threshold of the radial velocity standard deviation

Returns the current threshold of the radial velocity standard deviation
(`sd_vvp`) of an object in m/s, retrieved by velocity volume processing
(VVP).

## Usage

``` r
sd_vvp_threshold(x)

# S3 method for class 'vp'
sd_vvp_threshold(x)

# S3 method for class 'list'
sd_vvp_threshold(x)

# S3 method for class 'vpts'
sd_vvp_threshold(x)
```

## Arguments

- x:

  A `vp`, list of `vp` or `vpts` object.

## Value

The `sd_vvp` threshold in m/s.

## See also

- [`sd_vvp_threshold()<-`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold-set.md)
  for setting the `sd_vvp` threshold of an object.

- [`rcs()`](http://adriaandokter.com/bioRad/dev/reference/rcs.md)

## Examples

``` r
# Get the sd_vvp threshold for a vp
sd_vvp_threshold(example_vp)
#> [1] 2

# Get the sd_vvp threshold for a vpts
sd_vvp_threshold(example_vpts)
#> [1] 2
```
