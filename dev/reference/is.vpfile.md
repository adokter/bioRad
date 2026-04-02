# Check if a file is a vertical profile (`vp`)

Checks whether a file is a vertical profile (`vp`) in the ODIM HDF5
format that can be read with bioRad.

## Usage

``` r
is.vpfile(file)
```

## Arguments

- file:

  Character. Path of the file to check.

## Value

`TRUE` for a vertical profile file in readable format, otherwise
`FALSE`.

## See also

- [`read_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/read_vpfiles.md)

- [`get_odim_object_type()`](http://adriaandokter.com/bioRad/dev/reference/get_odim_object_type.md)

- [`is.vp()`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)

## Examples

``` r
# Locate the vertical profile example file
vpfile <- system.file("extdata", "profile.h5", package = "bioRad")

# Check if it is a vpfile
is.vpfile(vpfile)
#> [1] TRUE
```
