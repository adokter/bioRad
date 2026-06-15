# Download vertical profile (`vp`) files from the ENRAM data repository

**\[superseded\]**

This function has been superseded by
[`getRad::get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.html).

Download and unzip a selection of vertical profile (`vp`) files from the
[ENRAM data repository](https://aloftdata.eu/), where these are stored
as monthly zips per radar.

## Usage

``` r
download_vpfiles(
  date_min,
  date_max,
  radars,
  directory = ".",
  overwrite = FALSE
)
```

## Arguments

- date_min:

  Character. Start date of file selection, in `YYYY-MM-DD` format. Days
  will be ignored.

- date_max:

  Character. End date of file selection, in `YYYY-MM-DD` format. Days
  will be ignored.

- radars:

  Character (vector). 5-letter country/radar code(s) to include in file
  selection.

- directory:

  Character. Path to local directory where files should be downloaded
  and unzipped.

- overwrite:

  Logical. When `TRUE`, re-download and overwrite previously downloaded
  files of the same names.

## Value

`NULL`. The function's primary effect is to download selected vertical
profiles files from ENRAM data repository to a specified local
directory, and to provide a message and a progress bar in the console
indicating the download status. Message will show a 404 error for files
that are not available.

## See also

- [`read_vpts()`](http://adriaandokter.com/bioRad/reference/read_vpts.md)

- [`select_vpfiles()`](http://adriaandokter.com/bioRad/reference/select_vpfiles.md)

- [`read_vpfiles()`](http://adriaandokter.com/bioRad/reference/read_vpfiles.md)

- [`getRad::get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.html)

## Examples

``` r
# \donttest{
# Download (and overwrite) data from radars "bejab" and "bewid".
download_vpfiles(
  date_min = "2018-10-01",
  date_max = "2018-10-31",
  radars = c("bejab", "bewid"),
  directory = tempdir(),
  overwrite = TRUE
)
#> Warning: `download_vpfiles()` was deprecated in bioRad 2025.
#> ℹ Please use `getRad::get_vpts()` instead.
#> Downloading data from https://aloftdata.s3-eu-west-1.amazonaws.com
#> bejab_vpts_201810.csv.gz: successfully downloaded
#> bewid_vpts_201810.csv.gz: successfully downloaded
# }
```
