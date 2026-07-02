# Check if a file is a polar volume (`pvol`)

Checks whether a file is a polar volume (`pvol`) in the ODIM HDF5 format
that can be read with bioRad. Evaluates to `FALSE` for NEXRAD and IRIS
RAW polar volume file (see
[`nexrad_to_odim()`](http://adriaandokter.com/bioRad/dev/reference/nexrad_to_odim.md)).

## Usage

``` r
is.pvolfile(file)
```

## Arguments

- file:

  Character. Path of the file to check.

## Value

`TRUE` for a polar volume file in readable format, otherwise `FALSE`.

## See also

Other pvol read functions:
[`download_pvolfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_pvolfiles.md),
[`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md),
[`write_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/write_pvolfile.md)

## Examples

``` r
# Locate the polar volume example file
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")

# Check if it is a pvolfile
is.pvolfile(pvolfile)
#> [1] TRUE
```
