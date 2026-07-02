# Convert legacy bioRad objects

Convert legacy bioRad objects (`vp`, `vpts`) and make them compatible
with the current bioRad version. Conversion includes renaming `HGHT` to
`height`.

## Usage

``` r
convert_legacy(x)

# S3 method for class 'vp'
convert_legacy(x)

# S3 method for class 'vpts'
convert_legacy(x)
```

## Arguments

- x:

  A `vp` or `vpts` object.

## Value

An updated object of the same class as the input.

## See also

Other conversion functions:
[`dbz_to_eta()`](http://adriaandokter.com/bioRad/dev/reference/dbz_to_eta.md),
[`eta_to_dbz()`](http://adriaandokter.com/bioRad/dev/reference/eta_to_dbz.md),
[`nyquist_velocity()`](http://adriaandokter.com/bioRad/dev/reference/nyquist_velocity.md)

## Examples

``` r
# Convert a vp object
vp <- convert_legacy(example_vp)

# Convert a vpts object
vpts <- convert_legacy(example_vpts)
```
