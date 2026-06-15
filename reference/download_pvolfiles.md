# Download polar volume (`pvol`) files from the NEXRAD archive

Download a selection of polar volume (`pvol`) files from the [NEXRAD
Level II archive data](https://registry.opendata.aws/noaa-nexrad/).

## Usage

``` r
download_pvolfiles(
  date_min,
  date_max,
  radar,
  directory = ".",
  overwrite = FALSE,
  bucket = "unidata-nexrad-level2",
  directory_tree = TRUE,
  region = "us-east-1"
)
```

## Arguments

- date_min:

  POSIXct. Start date of file selection. If no timezone are provided, it
  will be assumed to be UTC.

- date_max:

  POSIXct. End date of file selection.If no timezone are provided, it
  will be assumed to be UTC.

- radar:

  character (vector). 4-letter radar code(s) (e.g. "KAMA")

- directory:

  character. Path to local directory where files should be downloaded

- overwrite:

  logical. TRUE to re-download and overwrite existing files

- bucket:

  character. Bucket name to use.

- directory_tree:

  logical. Whether to create the yyyy/mm/dd/radar directory structure.
  TRUE by default.

- region:

  character. AWS region used for S3 requests

## Value

`NULL`. The function's primary effect is to download selected polar
volume files from the NEXRAD Level II archive to a specified local
directory, and to provide a message and a progress bar in the console
indicating the download status.

## See also

[`getRad::get_pvol()`](https://aloftdata.github.io/getRad/reference/get_pvol.html)
for loading polar volumes files directly as a `pvol` object. This
function also provides access to some European radars.

## Examples

``` r
# \donttest{
# create temporary directory
temp_dir <- paste0(tempdir(),"/bioRad_tmp_files")
dir.create(temp_dir)
download_pvolfiles(
  date_min = as.POSIXct("2002-10-01 00:00", tz = "UTC"),
  date_max = as.POSIXct("2002-10-01 00:05", tz = "UTC"),
  radar = "KBRO",
  directory = temp_dir,
  overwrite = TRUE
)
#> Downloading data from unidata-nexrad-level2 for radar KBRO spanning over 1 days
#> 
#> Downloading pvol for 2002/10/01/KBRO/
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
# Clean up
unlink(temp_dir, recursive = TRUE)
# }
```
