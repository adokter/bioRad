# Partition mixtures of animals using assumptions on airspeeds.

Partition mixtures of birds and insects using assumptions on their
respective airspeeds, following the approach by Shi et al. (2025).

## Usage

``` r
clean_mixture(x, ...)

# Default S3 method
clean_mixture(
  x,
  slow = 1,
  fast = 8,
  drop_slow_component = TRUE,
  drop_missing = FALSE,
  keep_mixture = FALSE,
  u_wind,
  v_wind,
  u,
  v,
  ...
)

# S3 method for class 'vpts'
clean_mixture(
  x,
  slow = 1,
  fast = 8,
  drop_slow_component = TRUE,
  drop_missing = FALSE,
  keep_mixture = FALSE,
  u_wind = "u_wind",
  v_wind = "v_wind",
  ...
)

# S3 method for class 'vp'
clean_mixture(
  x,
  ...,
  slow = 1,
  fast = 8,
  drop_slow_component = TRUE,
  drop_missing = FALSE,
  keep_mixture = FALSE,
  u_wind = "u_wind",
  v_wind = "v_wind"
)
```

## Arguments

- x:

  a `vp` or `vpts` object, or a mixture animal density or linear
  reflectivity eta in cm\\^2\\/km\\^3\\.

- ...:

  `eta`, `u`, `v`, `u_wind`, `v_wind` arguments, taken from object for
  `vp` or `vpts` class.

- slow:

  the slow component's airspeed in m/s, typically the airspeed of
  insects. Either a single number, or (optionally for `vpts`) a numeric
  vector equal in length to the number of profiles, or a data column
  name (see Details).

- fast:

  the fast component's airspeed in m/s, typically the airspeed of birds.
  Either a single number, or (optionally for `vpts`) a numeric vector
  equal in length to the number of profiles, or a data column name (see
  Details).

- drop_slow_component:

  when TRUE (default) output density, ground speed and heading for fast
  component, when FALSE for slow component.

- drop_missing:

  Values `eta` without an associated ground speed and wind speed are set
  to NA when `TRUE`, or returned unaltered when `FALSE` (default).

- keep_mixture:

  When `TRUE` store original mixture reflectivity and speeds as renamed
  quantities with `mixture_` prefix

- u_wind:

  the west to east wind component in m/s. In the case of `vp` and `vpts`
  objects the quantity name for the U-component of the wind.

- v_wind:

  the south to north wind component in m/s. In the case of `vp` and
  `vpts` objects the quantity name for the V-component of the wind.

- u:

  the mixture's ground speed u component (west to east) in m/s.

- v:

  the mixture's ground speed v component (south to north) in m/s.

## Value

a named list with cleaned densities and speeds. Output differs depending
on whether the fast component is retained (`drop_slow_component`=`TRUE`,
default) or the slow component (`drop_slow_component`=`FALSE`, default).
Output quantities include:

- `eta`: cleaned reflectivity in cm^2/km^3. only the fast component
  (default) or the slow component (when `drop_slow_component` is
  `TRUE`).

- `u`: cleaned ground speed component west to east in m/s.

- `v`: cleaned ground speed component south to north in m/s.

- `airspeed`: the airspeed of the selected component in m/s.

- `airspeed_u`: the u-component (west to east) of the airspeed of the
  retained component in m/s.

- `airspeed_v`: the v-component (south to north) of the airspeed of the
  retained component in m/s.

- `heading`: the heading of the selected component in degrees clockwise
  from north.

- `f`: the reflectivity proportion of the slow component (0-1 range),
  typically the proportion of insects.

For `vp` and `vpts` objects the quantities `eta`,`u`,`v` will be
updated, and other quantities listed above will be added.

## Details

For a detail description of the methodology see Shi et al. (2025). Most
commonly the fast component refers to migrating birds, while the slow
component refers to insects. The slow component is always oriented in
the direction of the wind by definition. Note that for mixture airspeeds
exceeding the airspeed of the fast component, all reflectivity is
assigned to the fast component. Similarly, for mixture airspeeds below
the airspeed of the slow component, all reflectivity will be assigned to
the slow component.

### How to use this function?

1.  To apply this function to `vp` or `vpts` data altitudinal wind data
    needs to be added to the vertical profile data first. This is most
    easily accomplished by first converting the objects to a
    `data.frame` with
    [`as.vp()`](http://adriaandokter.com/bioRad/reference/as.vp.md) or
    [`as.vpts()`](http://adriaandokter.com/bioRad/reference/as.vpts.md).
    Wind data can then be added as a new columns to the data.frame. By
    default the wind data is expected to be named `u_wind` for the U
    component and `v_wind` for the V component of the wind.
    Alternatively, arguments `u_wind` and `v_wind` can be used to
    specify different names.

2.  Realistic assumptions for the expected airspeed for the slow
    (insect) and fast (bird) components need to be provided, using
    arguments `slow` and `fast`. See Shi et al. 2025 for recommendations
    in choosing these values. The parameter values for `fast` and `slow`
    can be specified as follows:

    - as single values applied to all heights and timestamps

    - as a numeric vector of equal length as the number of profiles in
      the `vpts`, allowing the user to specify changes in the parameter
      over time

    - as the name of a profile data quantity, allowing the user to
      specify changes in the parameter over time and/or altitude.
      Profile quantities are most easily added by first converting the
      `vpts` object to a data.frame with
      [`as.data.frame.vpts()`](http://adriaandokter.com/bioRad/reference/as.data.frame.vp.md),
      adding the values, and back-converting with
      [as.vpts](http://adriaandokter.com/bioRad/reference/as.vpts.md)

3.  Use `drop_slow_component` to toggle between retaining the slow or
    fast component. When `TRUE` the fast (bird) component is retained.
    When `FALSE` the slow (insect) component is retained. Note that in
    this case the corrected ground speed direction will be identical to
    the wind direction, and the magnitude of the ground speed will be
    equal to the wind speed plus the value of `slow`, due to the
    underlying assumption of wind following by the slow component.

## References

- Shi X, Drucker J, Chapman JW, Sanchez Herrera M, Dokter AM Analysis of
  mixtures of birds and insects in weather radar data. Ornithological
  Applications. 2025 (in press)
  [doi:10.1093/ornithapp/duaf020](https://doi.org/10.1093/ornithapp/duaf020)
  .

- Nussbaumer R, Schmid B, Bauer S, Liechti F. A Gaussian mixture model
  to separate birds and insects in single-polarization weather radar
  data. Remote Sensing. 2021 May 19;13(10):1989
  [doi:10.3390/rs13101989](https://doi.org/10.3390/rs13101989) .

## Examples

``` r
# convert profile object to data.frame
df <- as.data.frame(example_vp, suntime=FALSE)
# add wind u and v component wind data
# (here a NW wind identical at all altitudes)
df$u_wind=3
df$v_wind=-3
# convert back to vp object
my_vp <- as.vp(df)
#> Warning: Extra fields found: u_wind, v_wind
# partition the mixture:
my_vp_clean <- clean_mixture(my_vp)

# drop the slow component (typically insects)
clean_mixture(100,u=-13,v=13,u_wind=-7,v_wind=6, fast=8, slow=1)
#> $eta
#> [1] 100
#> 
#> $u
#> [1] -13
#> 
#> $v
#> [1] 13
#> 
#> $airspeed
#> [1] 9.219544
#> 
#> $heading
#> [1] -40.60129
#> 
#> $airspeed_u
#> [1] -6
#> 
#> $airspeed_v
#> [1] 7
#> 
#> $f
#> [1] 0
#> 
# drop the fast component (typically birds)
clean_mixture(100,u=-13,v=13,u_wind=-7,v_wind=6, fast=8, slow=1, drop_slow_component=FALSE)
#> $eta
#> [1] 0
#> 
#> $u
#> [1] -13
#> 
#> $v
#> [1] 13
#> 
#> $airspeed
#> [1] 9.219544
#> 
#> $heading
#> [1] -40.60129
#> 
#> $airspeed_u
#> [1] -6
#> 
#> $airspeed_v
#> [1] 7
#> 
#> $f
#> [1] 0
#> 
# keep the original mixture reflectivity and speed components
clean_mixture(100,u=-13,v=13,u_wind=-7,v_wind=6, fast=8, slow=1, keep_mixture=TRUE)
#> $eta
#> [1] 100
#> 
#> $u
#> [1] -13
#> 
#> $v
#> [1] 13
#> 
#> $airspeed
#> [1] 9.219544
#> 
#> $heading
#> [1] -40.60129
#> 
#> $airspeed_u
#> [1] -6
#> 
#> $airspeed_v
#> [1] 7
#> 
#> $f
#> [1] 0
#> 
#> $mixture_eta
#> [1] 100
#> 
#> $mixture_u
#> [1] -13
#> 
#> $mixture_v
#> [1] 13
#> 
#> $mixture_airspeed
#> [1] 9.219544
#> 
#> $mixture_heading
#> [1] 319.3987
#> 
# keep reflectivity unaltered when one of the speed components is not a number:
clean_mixture(100,u=-13,v=13,u_wind=NaN,v_wind=6, fast=8, slow=1)["eta"]
#> Warning: U component of wind contains non finite values
#> $eta
#> [1] 100
#> 
# set reflectivity to NaN when one of the speed components is not a number:
clean_mixture(100,u=-13,v=13,u_wind=NaN,v_wind=6, fast=8, slow=1, drop_missing=TRUE)["eta"]
#> Warning: U component of wind contains non finite values
#> $eta
#> [1] NaN
#> 
```
