# Check the `data` type of an ODIM HDF5 file

Checks what type of `data` object is contained in an ODIM HDF5 file. See
[ODIM
specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf),
Table 2 for a full list of existing ODIM file object types.

## Usage

``` r
get_odim_object_type(file)
```

## Arguments

- file:

  Character. Path of the file to check.

## Value

Character. `PVOL` for polar volume, `VP` for vertical profile, otherwise
`NA`.

## See also

- [`is.pvolfile()`](http://adriaandokter.com/bioRad/reference/is.pvolfile.md)

- [`is.vpfile()`](http://adriaandokter.com/bioRad/reference/is.vpfile.md)

## Examples

``` r
# Locate the polar volume example file
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")

# Check the data type
get_odim_object_type(pvolfile)
#> [1] "PVOL"
```
