# List aloft urls for time series of vertical profiles (`vpts`) of radar stations

**\[superseded\]**

This function has been superseded by
[`getRad::get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.html)
and
[`getRad::get_vpts_coverage()`](https://aloftdata.github.io/getRad/reference/get_vpts_coverage.html).

## Usage

``` r
list_vpts_aloft(
  date_min = NULL,
  date_max = NULL,
  radars = NULL,
  format = "csv",
  source = "baltrad",
  show_warnings = TRUE
)
```

## Arguments

- date_min:

  Character, the first date to return urls for. In the shape of
  YYYY-MM-DD.

- date_max:

  Character, the last date to return urls for. In the shape of
  YYYY-MM-DD.

- radars:

  Character vector, radar stations to return urls for.

- format:

  Character, the format of archive urls to return, either csv or hdf5.
  Currently only csv urls are supported.

- source:

  Character, either `baltrad` or `ecog-04003`

- show_warnings:

  Logical, whether to print warnings for dates or radar stations for
  which no data was found.

## Value

A character vector of aloft urls

## Examples

``` r
# \donttest{
list_vpts_aloft(radars = "bejab", date_min='2018-10-01', date_max = '2018-12-31')
#> Warning: `list_vpts_aloft()` was deprecated in bioRad 2025.
#> ℹ Please use `getRad::get_vpts()` instead.
#> Registered S3 methods overwritten by 'readr':
#>   method                    from 
#>   as.data.frame.spec_tbl_df vroom
#>   as_tibble.spec_tbl_df     vroom
#>   format.col_spec           vroom
#>   print.col_spec            vroom
#>   print.collector           vroom
#>   print.date_names          vroom
#>   print.locale              vroom
#>   str.col_spec              vroom
#> https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2018/bejab_vpts_201810.csv.gz
#> https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2018/bejab_vpts_201811.csv.gz
#> https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2018/bejab_vpts_201812.csv.gz
# }
```
