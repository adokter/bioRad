# Select vertical profile (`vp`) files from computer

Create a list of vertical profile (`vp`) files from a local directory
that match a specific date and radar range. Files are selected based on
their file name (not directory structure), which should be of format
`radar_vp_yyyymmdd*.*`, such as `bewid_vp_20171123T1900Z_0x5.h5`.

## Usage

``` r
select_vpfiles(
  date_min = NULL,
  date_max = NULL,
  radars = NULL,
  directory = "."
)
```

## Arguments

- date_min:

  character. YYYY-MM-DD start date of file selection.

- date_max:

  character. YYYY-MM-DD end date of file selection.

- radars:

  character (vector). 5-letter country/radar code(s) (e.g. `bejab`) of
  radars to include in file selection.

- directory:

  character. Path to local directory where files should be looked for.

## Value

Character vector of file paths that comply to the given date and radar
range.

## See also

download_vpfiles

## Examples

``` r
select_vpfiles(
  date_min = "2016-10-03",
  date_max = "2016-10-05",
  radars = "bejab",
  directory = "my_data"
)
#> character(0)
```
