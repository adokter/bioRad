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

- [`summary.vp()`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)

- [`summary.vpts()`](http://adriaandokter.com/bioRad/dev/reference/summary.vpts.md)

## Examples

``` r
# Convert a vp object
vp <- convert_legacy(example_vp)

# Convert a vpts object
vpts <- convert_legacy(example_vpts)
```
