# Regularize a time series of vertical profiles (`vpts`) on a regular time grid

Projects objects of class `vpts` on a regular time grid

## Usage

``` r
regularize_vpts(
  ts,
  interval = "auto",
  date_min,
  date_max,
  units = "secs",
  fill = TRUE,
  verbose = TRUE,
  keep_datetime = FALSE
)
```

## Arguments

- ts:

  An object inheriting from class `vpts`, see
  [`vpts()`](http://adriaandokter.com/bioRad/dev/reference/summary.vpts.md)
  for details.

- interval:

  Time interval grid to project on. When '`auto`' the median interval in
  the time series is used.

- date_min:

  Start time of the projected time series, as a POSIXct object. Taken
  from `ts` by default'.

- date_max:

  End time of the projected time series, as a POSIXct object. Taken from
  `ts` by default.

- units:

  Optional units of `interval` and `fill`, one of 'secs', 'mins',
  'hours','days', 'weeks'. Defaults to 'mins'.

- fill:

  Numeric or Logical. fill each regularized timestep with the closest
  original profile found within a time window of +/- `fill`. When
  `TRUE`, `fill` maps to `interval`, filling single missing timesteps.
  When `FALSE`, `fill` maps to 0, disabling filling.

- verbose:

  Logical, when `TRUE` prints text to console.

- keep_datetime:

  Logical, when `TRUE` keep original radar acquisition timestamps.

## Value

An object of class `vpts` with regular time steps.

## Details

Projects objects of class `vpts` on a regular time grid, and fills
temporal gaps by nearest neighbor interpolation.

Irregular time series of profiles are typically aligned on a regular
time grid with the expected time interval at which a radar provides
data. Alignment is performed using a nearest neighbor interpolation
limited to neighboring profiles that fall within +/- `fill` (centered)
of an original profile.

Remaining temporal gaps in the time series are filled with empty
profiles that have values NA for all quantities, such that each
timestamp of the regular grid has an associated profile.

In plots of regular time series (see
[`plot.vpts()`](http://adriaandokter.com/bioRad/dev/reference/plot.vpts.md))
temporal gaps of missing profiles (e.g. due to radar down time) become
visible, as a result of the gap filling with empty profiles. In
irregular time series data points in the plot are carried through until
the time series continues, and temporal data gaps are filled up
visually.

When `keep_datetime` is `TRUE` the original profile timestamps are kept
in `ts$datetime`. This may lead to duplicate timestamps when
regularizing on a timegrid finer than the interval of available
profiles.

## See also

Other profile manipulation functions:
[`bind_into_vpts()`](http://adriaandokter.com/bioRad/dev/reference/bind_into_vpts.md),
[`c.vp()`](http://adriaandokter.com/bioRad/dev/reference/c.vp.md),
[`clean_mixture()`](http://adriaandokter.com/bioRad/dev/reference/clean_mixture.md),
[`filter_precip()`](http://adriaandokter.com/bioRad/dev/reference/filter_precip.md),
[`filter_vpts()`](http://adriaandokter.com/bioRad/dev/reference/filter_vpts.md),
[`rcs<-()`](http://adriaandokter.com/bioRad/dev/reference/rcs-set.md),
[`sd_vvp_threshold<-()`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold-set.md)

## Examples

``` r
# start form example vpts object:
data(example_vpts)
ts <- example_vpts
# data gaps are not visible:
plot(ts)
#> Warning: Irregular time-series: missing profiles will not be visible. Use 'regularize_vpts' to make time series regular.


# regularize the time series on a 5 minute interval grid
tsRegular <- regularize_vpts(ts, interval = 300)
# data gaps are visible:
plot(tsRegular)


# regularize the time series on a 10 minute interval grid,
# and fill data gaps smaller than 1 hour by nearest neighbor interpolation
tsRegular <- regularize_vpts(ts, interval = 600, fill = 3600)
# data gaps are smaller as a result of nearest neighbor interpolation:
plot(tsRegular)
```
