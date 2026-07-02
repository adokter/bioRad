# Set radar cross section

Sets the assumed radar cross section of an object in cm^2. This function
also updates the migration densities in `x$data$dens` to `eta`/`rcs`
when above `sd_vvp_threshold` and `0` if below.

## Usage

``` r
rcs(x) <- value

# S3 method for class 'vp'
rcs(x) <- value

# S3 method for class 'list'
rcs(x) <- value

# S3 method for class 'vpts'
rcs(x) <- value

# S3 method for class 'vpi'
rcs(x) <- value
```

## Arguments

- x:

  A `vp`, list of `vp`, `vpts` or `vpi` object.

- value:

  Numeric. The radar cross section value to assign in cm^2.

## Value

The input object with updated density `x$data$dens` and updated radar
cross section attribute.

## See also

Other profile manipulation functions:
[`bind_into_vpts()`](http://adriaandokter.com/bioRad/dev/reference/bind_into_vpts.md),
[`c.vp()`](http://adriaandokter.com/bioRad/dev/reference/c.vp.md),
[`clean_mixture()`](http://adriaandokter.com/bioRad/dev/reference/clean_mixture.md),
[`filter_precip()`](http://adriaandokter.com/bioRad/dev/reference/filter_precip.md),
[`filter_vpts()`](http://adriaandokter.com/bioRad/dev/reference/filter_vpts.md),
[`regularize_vpts()`](http://adriaandokter.com/bioRad/dev/reference/regularize_vpts.md),
[`sd_vvp_threshold<-()`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold-set.md)

## Examples

``` r
# Set the radar cross section for a vp
vp <- example_vp
rcs(vp) <- 11

# Set the radar cross section for a vpts
vpts <- example_vpts
rcs(vpts) <- 11

# Set the radar cross section for a vpi
vpi <- integrate_profile(example_vpts)
rcs(vpi) <- 11
```
