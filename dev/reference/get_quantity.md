# Get a quantity from a vertical profile (`vp`) or time series of vertical profiles (`vpts`)

Returns values for the selected quantity from a vertical profile (`vp`),
list, or time series of vertical profiles (`vpts`). Values are organized
per height bin. Values for `eta` are set to `0`, `dbz` to `-Inf` and
`ff`, `u`, `v`, `w`, `dd` to `NaN` when the `sd_vvp` for that height bin
is below the
[`sd_vvp_threshold()`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold.md).

## Usage

``` r
get_quantity(x, quantity)

# S3 method for class 'vp'
get_quantity(x, quantity = "dens")

# S3 method for class 'list'
get_quantity(x, quantity = "dens")

# S3 method for class 'vpts'
get_quantity(x, quantity = "dens")
```

## Arguments

- x:

  A `vp`, list of `vp` or `vpts` object.

- quantity:

  Character. A (case sensitive) profile quantity, one of:

  - `height`: Height bin (lower bound) in m above sea level.

  - `u`: Ground speed component west to east in m/s.

  - `v`: Ground speed component south to north in m/s.

  - `w`: Vertical speed (unreliable!) in m/s.

  - `ff`: Horizontal speed in m/s.

  - `dd`: Direction in degrees clockwise from north.

  - `sd_vvp`: VVP radial velocity standard deviation in m/s.

  - `gap`: Angular data gap detected in T/F.

  - `dbz`: Animal reflectivity factor in dBZ.

  - `eta`: Animal reflectivity in cm^2/km^3.

  - `dens`: Animal density in animals/km^3.

  - `DBZH`: Total reflectivity factor (bio + meteo scattering) in dBZ.

  - `n`: Number of data points used for the ground speed estimates
    (quantities `u`, `v`, `w`, `ff`, `dd`).

  - `n_all`: Number of data points used for the radial velocity standard
    deviation estimate (quantity `sd_vvp`).

  - `n_dbz`: Number of data points used for reflectivity-based estimates
    (quantities `dbz`, `eta`, `dens`).

  - `n_dbz_all`: Number of data points used for the total reflectivity
    estimate (quantity `DBZH`).

  - `attributes`: List of the vertical profile's `what`, `where` and
    `how` attributes.

## Value

the value of a specific profile quantity specified in `quantity`.

For a `vp` object: a named (height bin) vector with values for the
selected quantity.

For a `list` object: a list of named (height bin) vectors with values
for the selected quantity.

For a `vpts` object: a (height bin \* datetime) matrix with values for
the selected quantity.

## See also

Use
[`sd_vvp_threshold()<-`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold-set.md)
for setting the `sd_vvp` threshold of an object.

Other vp functions:
[`example_vp`](http://adriaandokter.com/bioRad/dev/reference/example_vp.md),
[`plot.vp()`](http://adriaandokter.com/bioRad/dev/reference/plot.vp.md),
[`summary.vp()`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)

## Examples

``` r
# Extract the animal density (dens) quantity from a vp object
get_quantity(example_vp, "dens")
#>           0         200         400         600         800        1000 
#>          NA  85.0995178 110.4298782  91.7822418  34.5677528  20.5497875 
#>        1200        1400        1600        1800        2000        2200 
#>  22.9609985  19.7929668  19.7772617  17.3985100  11.9922190   7.2263165 
#>        2400        2600        2800        3000        3200        3400 
#>   3.0735207   0.0000000   0.7724188   0.0000000   0.0000000   0.0000000 
#>        3600        3800        4000        4200        4400        4600 
#>   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000 
#>        4800 
#>   0.0000000 

# Extract the horizontal ground speed (ff) quantity from a vpts object and show the
# first two datetimes
get_quantity(example_vpts, "ff")[,1:2]
#>      2016-09-01 00:02:00 2016-09-01 00:06:00
#> 0                    NaN                 NaN
#> 200                  NaN                 NaN
#> 400                  NaN                 NaN
#> 600                 5.65                5.23
#> 800                 5.07                5.64
#> 1000                5.45                5.39
#> 1200                6.02                6.20
#> 1400                5.96                5.81
#> 1600                4.82                4.63
#> 1800                4.61                4.07
#> 2000                 NaN                2.74
#> 2200                 NaN                2.28
#> 2400                 NaN                0.73
#> 2600                 NaN                0.74
#> 2800                 NaN                 NaN
#> 3000                 NaN                 NaN
#> 3200                 NaN                 NaN
#> 3400                 NaN                1.11
#> 3600                 NaN                1.17
#> 3800                 NaN                 NaN
#> 4000                 NaN                 NaN
#> 4200                 NaN                 NaN
#> 4400                 NaN                 NaN
#> 4600                 NaN                 NaN
#> 4800                 NaN                 NaN
```
