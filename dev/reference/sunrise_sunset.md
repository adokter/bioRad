# Calculate sunrise or sunset for a time and place

Calculate sunrise or sunset for a time and place

## Usage

``` r
sunrise(date, lon, lat, elev = -0.268, tz = "UTC", force_tz = FALSE)

sunset(date, lon, lat, elev = -0.268, tz = "UTC", force_tz = FALSE)
```

## Arguments

- date:

  POSIXct. Date interpretable by
  [`base::as.Date()`](https://rdrr.io/r/base/as.Date.html).

- lon:

  Numeric. Longitude in decimal degrees.

- lat:

  Numeric. Latitude in decimal degrees.

- elev:

  Numeric. Sun elevation in degrees.

- tz:

  Character. Time zone of `date`, ignored if `date` already has an
  associated time zone.

- force_tz:

  Logical. If `TRUE`, the output is converted to the timezone set by
  `tz`.

## Value

The moment of sunrise or sunset for the date set by `date`and time zone
as specified (by `date` and `tz`) or in UTC if not specified.

## Details

The day for which sunrise and sunset are calculated is given by the
input date. Sunrise and sunset are calculated relative to the moment of
solar noon for that date, i.e. the first sunrise before the moment of
solar noon, and the first sunset after the moment of solar noon.
Therefore, depending on the timezone provided, it is possible that the
nearest sunrise prior to solar noon occurs a day earlier than the input
date. Similarly, sunset may occur a day later than the input date. See
examples for details.

The angular diameter of the sun is about 0.536 degrees, therefore the
moment of sunrise/sunset corresponds to half that elevation at -0.268
degrees.

This is a convenience function mapping to
[suntools::crepuscule](https://rdrr.io/pkg/suntools/man/crepuscule.html).

Approximate astronomical formula are used, therefore the moment of
sunrise / sunset may be off by a few minutes

If `force_tz` is `TRUE`, the output is converted to the timezone set by
`tz`

## Examples

``` r
# sunrise in the Netherlands
sunrise("2016-01-01", 5, 53)
#> [1] "2016-01-01 07:58:17 UTC"

# sunset in the Netherlands
sunset("2016-01-01", 5, 53)
#> [1] "2016-01-01 15:28:31 UTC"

# civil twilight in Ithaca, NY
sunrise("2016-01-01", -76.5, 42.4, elev = -6)
#> [1] "2016-01-01 12:03:29 UTC"

# next sunset in South Dakota, USA
sunset("2016-11-15", -98, 45)
#> [1] "2016-11-15 22:58:56 UTC"

# Beware that some days have two sunsets, or
# two sunrises! E.g. on 5 Oct (local timezone) at
# this location  sunset is actually on the 6 Oct
# in UTC time zone, i.e. the next day
sunset("2016-10-5", -98, 45)
#> [1] "2016-10-06 00:00:46 UTC"
# One day later, sunset is again on 6 Oct:
sunset("2016-10-6", -98, 45)
#> [1] "2016-10-06 23:58:56 UTC"

# working in local time zones typically avoids such ambiguities:
sunset(lubridate::as_datetime("2016-06-05",tz="America/Chicago"), -98, 45)
#> [1] "2016-06-06 02:11:19 UTC"
sunset(lubridate::as_datetime("2016-06-06",tz="America/Chicago"), -98, 45)
#> [1] "2016-06-07 02:12:00 UTC"

# use force_tz to force output to a specific time zone, by default UTC:
sunset(lubridate::as_datetime("2016-06-05",tz="America/Chicago"), -98, 45, force_tz=TRUE)
#> [1] "2016-06-06 02:11:19 UTC"
sunset(lubridate::as_datetime("2016-06-06",tz="America/Chicago"), -98, 45, force_tz=TRUE)
#> [1] "2016-06-07 02:12:00 UTC"

# Also beware of jumps in sunrise and sunset date with longitude:
sunrise("2016-11-01", 100, 45)
#> [1] "2016-11-01 00:01:50 UTC"
sunrise("2016-11-01", 102, 45)
#> [1] "2016-10-31 23:53:50 UTC"
```
