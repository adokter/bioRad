# Write a polar volume (`pvol`) object to ODIM HDF5 file

Write a polar volume (`pvol`) object to ODIM HDF5 file

## Usage

``` r
write_pvolfile(pvol, file, overwrite = FALSE, infer_dtype = FALSE)
```

## Arguments

- pvol:

  An object of class `pvol`.

- file:

  string. A filepath to write the `pvol` object to.

- overwrite:

  logical. Overwrites existing file when TRUE.

- infer_dtype:

  logical. By default (infer_dtype = FALSE) writes 'params' back into
  ODIM HDF5 files with data stored in original data types. When TRUE
  infers data type from the R object data type, at the cost of (heavily)
  inflated file sizes.

## Value

0 on success. A `pvol` object will be written to file in ODIM H5 format.

## See also

Other pvol read functions:
[`download_pvolfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_pvolfiles.md),
[`is.pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/is.pvolfile.md),
[`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md)

## Examples

``` r
# locate example volume file:
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")

# load the file:
example_pvol <- read_pvolfile(pvolfile)

# write the file:
pvolfile_out <- paste0(tempdir(),"pvolfile_out.h5")
write_pvolfile(example_pvol, pvolfile_out)

# clean up
file.remove(pvolfile_out)
#> [1] TRUE
```
