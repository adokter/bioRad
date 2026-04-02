# Set threshold of the radial velocity standard deviation

Sets the threshold of radial velocity standard deviation (`sd_vvp`) of
an object in m/s. Altitude layers with `sd_vvp` below this threshold are
assumed to have an aerial density of zero individuals. This function
also updates the migration densities in `x$data$dens` to `eta`/`rcs`
when above `sd_vvp_threshold` and `0` if below.

## Usage

``` r
sd_vvp_threshold(x) <- value

# S3 method for class 'vp'
sd_vvp_threshold(x) <- value

# S3 method for class 'list'
sd_vvp_threshold(x) <- value

# S3 method for class 'vpts'
sd_vvp_threshold(x) <- value
```

## Arguments

- x:

  A `vp`, list of `vp` or `vpts` object.

- value:

  Numeric. The `sd_vvp` threshold value to assign in m/s.

## Value

The input object with updated density `x$data$dens` and `sd_vvp_thresh`
attribute.

## See also

- [`sd_vvp_threshold()`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold.md)
  for getting the `sd_vvp` threshold of an object.

- [`rcs()<-`](http://adriaandokter.com/bioRad/dev/reference/rcs-set.md)

## Examples

``` r
# Set the sd_vvp threshold for a vp
vp <- example_vp
sd_vvp_threshold(vp) <- 2

# Set the sd_vvp threshold for a vpts
vpts <- example_vpts
sd_vvp_threshold(vpts) <- 2
```
