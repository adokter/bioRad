# Check if a file is a polar volume (`pvol`)

Checks whether a file is a polar volume (`pvol`) in the ODIM HDF5 format
that can be read with bioRad. Evaluates to `FALSE` for NEXRAD and IRIS
RAW polar volume file (see
[`nexrad_to_odim()`](http://adriaandokter.com/bioRad/reference/nexrad_to_odim.md)).

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

- [`read_pvolfile()`](http://adriaandokter.com/bioRad/reference/read_pvolfile.md)

- [`get_odim_object_type()`](http://adriaandokter.com/bioRad/reference/get_odim_object_type.md)

- [`is.pvol()`](http://adriaandokter.com/bioRad/reference/summary.pvol.md)

## Examples

``` r
# Locate the polar volume example file
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")

# Check if it is a pvolfile
is.pvolfile(pvolfile)
#> [1] TRUE
```
