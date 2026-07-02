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

Other profile read functions:
[`download_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_vpfiles.md),
[`list_vpts_aloft()`](http://adriaandokter.com/bioRad/dev/reference/list_vpts_aloft.md),
[`read_cajun()`](http://adriaandokter.com/bioRad/dev/reference/read_cajun.md),
[`read_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/read_vpfiles.md),
[`read_vpts()`](http://adriaandokter.com/bioRad/dev/reference/read_vpts.md),
[`select_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/select_vpfiles.md)

## Examples

``` r
# Locate the vertical profile example file
vpfile <- system.file("extdata", "profile.h5", package = "bioRad")

# Check if it is a vpfile
is.vpfile(vpfile)
#> [1] TRUE
```
