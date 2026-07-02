# Read time series of vertical profiles (`vpts`) from file(s)

Reads `vpts` data from one or more files. The following file formats are
supported (but cannot be mixed):

- [VPTS CSV](https://aloftdata.eu/vpts-csv/).

- [ODIM bird
  profile](https://github.com/adokter/vol2bird/wiki/ODIM-bird-profile-format-specification).

- vol2bird standard output (see example below).

## Usage

``` r
read_vpts(files, data_frame = FALSE, ...)
```

## Arguments

- files:

  Path(s) to one or more files containing vpts data.

- data_frame:

  When `FALSE` (default) output a `vpts` object, when `TRUE` output a
  data.frame

- ...:

  Additional arguments for backward compatibility, passed to
  `read_stdout`.

## Value

`vpts` object.

## See also

Other profile read functions:
[`download_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_vpfiles.md),
[`is.vpfile()`](http://adriaandokter.com/bioRad/dev/reference/is.vpfile.md),
[`list_vpts_aloft()`](http://adriaandokter.com/bioRad/dev/reference/list_vpts_aloft.md),
[`read_cajun()`](http://adriaandokter.com/bioRad/dev/reference/read_cajun.md),
[`read_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/read_vpfiles.md),
[`select_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/select_vpfiles.md)

## Examples

``` r
## read a vertical profile time series in VPTS CSV format:
vptsfile <- system.file("extdata", "example_vpts.csv", package = "bioRad")
read_vpts(vptsfile)
#> Warning: Validation issues found: Type validation failed for vcp
#> Warning: Validation issues found: Type validation failed for vcp
#>                    Regular time series of vertical profiles (class vpts)
#> 
#>            radar:  bewid 
#>       # profiles:  49 
#> time range (UTC):  2023-05-03 18:00:00 - 2023-05-04 06:00:00 
#>    time step (s):  900 
# read a single vertical profile file in ODIM h5 format:
vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
read_vpts(vpfile)
#>                    Irregular time series of vertical profiles (class vpts)
#> 
#>            radar:  seang 
#>       # profiles:  1 
#> time range (UTC):  2015-10-18 18:00:00 - 2015-10-18 18:00:00 
#>    time step (s):  min: NA     max:  NA 
# read a vertical profile time series in `vol2bird` stdout format:
stdout_file <- system.file("extdata", "example_vpts.txt", package = "bioRad")
read_vpts(stdout_file, radar = "KBGM", wavelength = "S")
#> Warning: .txt extenstion detected - falling back to read_stdout().
#> 
#>     Please consider updating your workflow by using VPTS csv or h5 input files
#>                    Irregular time series of vertical profiles (class vpts)
#> 
#>            radar:  KBGM 
#>       # profiles:  11 
#> time range (UTC):  2016-09-02 00:16:00 - 2016-09-02 01:52:00 
#>    time step (s):  min: 540     max:  600 
```
