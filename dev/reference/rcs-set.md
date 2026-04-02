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

- [`rcs()`](http://adriaandokter.com/bioRad/dev/reference/rcs.md) for
  getting the radar cross section of an object.

- [`sd_vvp_threshold()<-`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold-set.md)

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
