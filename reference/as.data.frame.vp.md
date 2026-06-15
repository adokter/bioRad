# Convert a vertical profile (`vp`) or time series of vertical profiles (`vpts`) to a data frame

Converts a vertical profile (`vp`) or a time series of vertical profiles
(`vpts`) to a data frame containing all quantities per datetime and
height. Has options to include latitude/longitude/antenna height
(parameter `geo`) and day/sunrise/sunset (parameter `suntime`).

## Usage

``` r
# S3 method for class 'vp'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  geo = TRUE,
  suntime = TRUE,
  lat = NULL,
  lon = NULL,
  elev = -0.268,
  ...
)

# S3 method for class 'vpts'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  geo = TRUE,
  suntime = TRUE,
  lat = NULL,
  lon = NULL,
  elev = -0.268,
  ...
)
```

## Arguments

- x:

  A `vp` or `vpts` object.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed. See
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- optional:

  Logical. If `FALSE` then the names of the variables in the data frame
  are checked to ensure that they are syntactically valid variable names
  and are not duplicated. See
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- geo:

  Logical. When `TRUE`, adds latitude (`lat`), longitude (`lon`) and
  antenna height of the radar (`height_antenna`) to each row.

- suntime:

  Logical. When `TRUE`, adds whether it is daytime (`day`) and the
  datetime of `sunrise` and `sunset` to each row.

- lat:

  Numeric. Radar latitude in decimal degrees. When set, overrides the
  latitude stored in `x` for
  [`sunrise()`](http://adriaandokter.com/bioRad/reference/sunrise_sunset.md)/[`sunset()`](http://adriaandokter.com/bioRad/reference/sunrise_sunset.md)
  calculations.

- lon:

  Numeric. Radar longitude in decimal degrees. When set, overrides the
  longitude stored in `x` for
  [`sunrise()`](http://adriaandokter.com/bioRad/reference/sunrise_sunset.md)/[`sunset()`](http://adriaandokter.com/bioRad/reference/sunrise_sunset.md)
  calculations.

- elev:

  Numeric. Sun elevation in degrees, used for
  [`sunrise()`](http://adriaandokter.com/bioRad/reference/sunrise_sunset.md)/[`sunset()`](http://adriaandokter.com/bioRad/reference/sunrise_sunset.md)
  calculations.

- ...:

  Additional arguments to be passed to or from methods.

## Value

A `data.frame` object, containing radar, datetime and height as rows and
all profile quantities as columns, complemented with some oft-used
additional information (columns `lat`, `lon`, `height_antenna`, `day`,
`sunrise`, `sunset`).

## Details

Note that only the `dens` quantity is thresholded for radial velocity
standard deviation by
[`sd_vvp_threshold()`](http://adriaandokter.com/bioRad/reference/sd_vvp_threshold.md).
This is different from the default
[`plot.vp()`](http://adriaandokter.com/bioRad/reference/plot.vp.md),
[`plot.vpts()`](http://adriaandokter.com/bioRad/reference/plot.vpts.md)
and
[`get_quantity()`](http://adriaandokter.com/bioRad/reference/get_quantity.md)
functions, where quantities `eta`, `dbz`, `ff`, `u`, `v`, `w`, `dd` are
all thresholded by
[`sd_vvp_threshold()`](http://adriaandokter.com/bioRad/reference/sd_vvp_threshold.md).

## See also

- [`summary.vpts()`](http://adriaandokter.com/bioRad/reference/summary.vpts.md)

## Examples

``` r
# Convert vp object to a data.frame
vp_df <- as.data.frame(example_vp)

# Print data.frame
vp_df
#>    radar            datetime       ff         dbz        dens         u
#> 1  seang 2015-10-18 18:00:00       NA          NA          NA        NA
#> 2  seang 2015-10-18 18:00:00 11.53461   4.3039665  85.0995178 -3.860118
#> 3  seang 2015-10-18 18:00:00 13.35705   5.4355612 110.4298782 -5.824306
#> 4  seang 2015-10-18 18:00:00 13.77482   4.6322823  91.7822418 -6.317519
#> 5  seang 2015-10-18 18:00:00 12.37280   0.3914067  34.5677528 -5.464484
#> 6  seang 2015-10-18 18:00:00 11.62856  -1.8672314  20.5497875 -5.435530
#> 7  seang 2015-10-18 18:00:00 12.26567  -1.3853970  22.9609985 -5.871046
#> 8  seang 2015-10-18 18:00:00 12.44552  -2.0301955  19.7929668 -5.042855
#> 9  seang 2015-10-18 18:00:00 12.72011  -2.0336432  19.7772617 -4.089148
#> 10 seang 2015-10-18 18:00:00 13.16166  -2.5901840  17.3985100 -4.711092
#> 11 seang 2015-10-18 18:00:00 13.24782  -4.2063093  11.9922190 -4.695442
#> 12 seang 2015-10-18 18:00:00 12.97676  -6.4061351   7.2263165 -4.695178
#> 13 seang 2015-10-18 18:00:00 13.96152 -10.1189432   3.0735207 -5.040771
#> 14 seang 2015-10-18 18:00:00      NaN -14.6137285   0.0000000       NaN
#> 15 seang 2015-10-18 18:00:00 18.53717 -16.1167755   0.7724188 -6.448045
#> 16 seang 2015-10-18 18:00:00      NaN -16.8988857   0.0000000       NaN
#> 17 seang 2015-10-18 18:00:00      NaN -28.0253792   0.0000000       NaN
#> 18 seang 2015-10-18 18:00:00      NaN -37.8717766   0.0000000       NaN
#> 19 seang 2015-10-18 18:00:00      NaN -38.7156448   0.0000000       NaN
#> 20 seang 2015-10-18 18:00:00      NaN -38.6279716   0.0000000       NaN
#> 21 seang 2015-10-18 18:00:00      NaN -39.0613213   0.0000000       NaN
#> 22 seang 2015-10-18 18:00:00      NaN -41.9553452   0.0000000       NaN
#> 23 seang 2015-10-18 18:00:00      NaN -39.9685364   0.0000000       NaN
#> 24 seang 2015-10-18 18:00:00      NaN -44.7469788   0.0000000       NaN
#> 25 seang 2015-10-18 18:00:00      NaN -40.6572495   0.0000000       NaN
#>            v   gap          w n_dbz       dd    n       DBZH height n_dbz_all
#> 1         NA  TRUE         NA     0       NA    0         NA      0         0
#> 2  -10.86953 FALSE  -8.255996  9786 199.5516 4384   4.853236    200     27593
#> 3  -12.02032 FALSE  -3.870171 10807 205.8520 5680   4.977803    400     24154
#> 4  -12.24070 FALSE -15.429576 10874 207.2986 5456   4.296990    600     18282
#> 5  -11.10070 FALSE  -7.646388  9343 206.2094 4067   0.749139    800     13321
#> 6  -10.28000 FALSE   2.433591  9025 207.8676 3628  -1.486296   1000     10471
#> 7  -10.76928 FALSE   3.606835  6743 208.5977 3126  -1.220092   1200      7308
#> 8  -11.37807 FALSE   2.602238  6873 203.9033 3180  -2.030195   1400      6873
#> 9  -12.04492 FALSE   1.397334  6516 198.7519 2962  -2.033643   1600      6516
#> 10 -12.28962 FALSE   5.478257  5871 200.9738 2284  -2.590184   1800      5871
#> 11 -12.38780 FALSE   6.864256  2960 200.7586 1034  -4.206309   2000      2960
#> 12 -12.09759 FALSE   4.974006  3640 201.2117  859  -6.406135   2200      3640
#> 13 -13.01978 FALSE   0.743979  3989 201.1645  374 -10.118943   2400      3989
#> 14       NaN  TRUE        NaN  2998      NaN  125 -14.613729   2600      2998
#> 15 -17.37957 FALSE  -2.458026  3656 200.3555   72 -16.116776   2800      3656
#> 16       NaN  TRUE        NaN  3660      NaN   51 -16.898886   3000      3660
#> 17       NaN  TRUE        NaN  3995      NaN    2 -28.025379   3200      3995
#> 18       NaN  TRUE        NaN  3656      NaN    0 -37.871777   3400      3656
#> 19       NaN  TRUE        NaN  2989      NaN    0 -38.715645   3600      2989
#> 20       NaN  TRUE        NaN  2654      NaN    0 -38.627972   3800      2654
#> 21       NaN  TRUE        NaN  1991      NaN    0 -39.061321   4000      1991
#> 22       NaN  TRUE        NaN  1991      NaN    0 -41.955345   4200      1991
#> 23       NaN  TRUE        NaN  2326      NaN    0 -39.968536   4400      2326
#> 24       NaN  TRUE        NaN  2319      NaN    0 -44.746979   4600      2319
#> 25       NaN  TRUE        NaN  1991      NaN    0 -40.657249   4800      1991
#>             eta   sd_vvp n_all rcs sd_vvp_threshold radar_latitude
#> 1            NA       NA     0  11                2        56.3675
#> 2  9.360947e+02 4.349111 11472  11                2        56.3675
#> 3  1.214729e+03 4.015257  9936  11                2        56.3675
#> 4  1.009605e+03 3.286927  8026  11                2        56.3675
#> 5  3.802453e+02 3.546942  5282  11                2        56.3675
#> 6  2.260477e+02 3.821268  4078  11                2        56.3675
#> 7  2.525710e+02 3.926216  3304  11                2        56.3675
#> 8  2.177226e+02 3.797443  3180  11                2        56.3675
#> 9  2.175499e+02 3.670397  2962  11                2        56.3675
#> 10 1.913836e+02 3.618254  2284  11                2        56.3675
#> 11 1.319144e+02 3.535195  1034  11                2        56.3675
#> 12 7.948948e+01 3.134931   859  11                2        56.3675
#> 13 3.380873e+01 2.994742   374  11                2        56.3675
#> 14 1.201020e+01      NaN   125  11                2        56.3675
#> 15 8.496607e+00 4.066769    72  11                2        56.3675
#> 16 7.096342e+00      NaN    51  11                2        56.3675
#> 17 5.475014e-01      NaN     2  11                2        56.3675
#> 18 5.672123e-02      NaN     0  11                2        56.3675
#> 19 4.670447e-02      NaN     0  11                2        56.3675
#> 20 4.765693e-02      NaN     0  11                2        56.3675
#> 21 4.313115e-02      NaN     0  11                2        56.3675
#> 22 2.215075e-02      NaN     0  11                2        56.3675
#> 23 3.500013e-02      NaN     0  11                2        56.3675
#> 24 1.164730e-02      NaN     0  11                2        56.3675
#> 25 2.986746e-02      NaN     0  11                2        56.3675
#>    radar_longitude radar_height radar_wavelength   day             sunrise
#> 1          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 2          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 3          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 4          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 5          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 6          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 7          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 8          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 9          12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 10         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 11         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 12         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 13         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 14         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 15         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 16         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 17         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 18         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 19         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 20         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 21         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 22         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 23         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 24         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#> 25         12.8517          209         5.348661 FALSE 2015-10-18 05:50:07
#>                 sunset
#> 1  2015-10-18 15:56:28
#> 2  2015-10-18 15:56:28
#> 3  2015-10-18 15:56:28
#> 4  2015-10-18 15:56:28
#> 5  2015-10-18 15:56:28
#> 6  2015-10-18 15:56:28
#> 7  2015-10-18 15:56:28
#> 8  2015-10-18 15:56:28
#> 9  2015-10-18 15:56:28
#> 10 2015-10-18 15:56:28
#> 11 2015-10-18 15:56:28
#> 12 2015-10-18 15:56:28
#> 13 2015-10-18 15:56:28
#> 14 2015-10-18 15:56:28
#> 15 2015-10-18 15:56:28
#> 16 2015-10-18 15:56:28
#> 17 2015-10-18 15:56:28
#> 18 2015-10-18 15:56:28
#> 19 2015-10-18 15:56:28
#> 20 2015-10-18 15:56:28
#> 21 2015-10-18 15:56:28
#> 22 2015-10-18 15:56:28
#> 23 2015-10-18 15:56:28
#> 24 2015-10-18 15:56:28
#> 25 2015-10-18 15:56:28

# Convert vpts object to a data.frame
vpts_df <- as.data.frame(example_vpts)

# Print the first 5 rows of the data.frame
vpts_df[1:5, ]
#>   radar            datetime height    u    v     w   ff   dd sd_vvp   gap   dbz
#> 1  KBGM 2016-09-01 00:02:00      0  NaN  NaN   NaN  NaN  NaN    NaN  TRUE   NaN
#> 2  KBGM 2016-09-01 00:02:00    200  NaN  NaN   NaN  NaN  NaN    NaN  TRUE   NaN
#> 3  KBGM 2016-09-01 00:02:00    400  NaN  NaN   NaN  NaN  NaN   2.81  TRUE  1.54
#> 4  KBGM 2016-09-01 00:02:00    600 4.14 3.84 12.17 5.65 47.2   2.80 FALSE  3.36
#> 5  KBGM 2016-09-01 00:02:00    800 5.06 0.24 15.17 5.07 87.2   2.42 FALSE -7.89
#>    eta      dens   DBZH    n n_dbz n_all n_dbz_all rcs sd_vvp_threshold
#> 1  NaN       NaN    NaN    0     0     0         0  11                2
#> 2  NaN       NaN    NaN    0     0     0         0  11                2
#> 3 30.8 2.8000000   3.77  326   356 22485     28416  11                2
#> 4 46.9 4.2636364   0.50 9006 13442 65947    104455  11                2
#> 5  3.5 0.3181818 -10.33 2313 11145 21321     63542  11                2
#>   radar_latitude radar_longitude radar_height radar_wavelength   day
#> 1       42.19972       -75.98472          519             10.6 FALSE
#> 2       42.19972       -75.98472          519             10.6 FALSE
#> 3       42.19972       -75.98472          519             10.6 FALSE
#> 4       42.19972       -75.98472          519             10.6 FALSE
#> 5       42.19972       -75.98472          519             10.6 FALSE
#>               sunrise              sunset
#> 1 2016-09-01 10:32:55 2016-09-01 23:33:48
#> 2 2016-09-01 10:32:55 2016-09-01 23:33:48
#> 3 2016-09-01 10:32:55 2016-09-01 23:33:48
#> 4 2016-09-01 10:32:55 2016-09-01 23:33:48
#> 5 2016-09-01 10:32:55 2016-09-01 23:33:48

# Do not add lat/lon/height_antenna information
vpts_df <- as.data.frame(example_vpts, geo = FALSE)

# Do not add day/sunrise/sunset information
vpts_df <- as.data.frame(example_vpts, suntime = FALSE)

# Override the latitude/longitude information stored in the object when
# calculating sunrise/sunset information
vpts_df <- as.data.frame(example_vpts, lat = 50, lon = 4)
```
