#' Calculate sunrise or sunset for a time and place
#'
#' @param date Date inheriting from class `POSIXt` or a string
#' interpretable by [as.Date][base::as.Date].
#' @param lon Longitude in decimal degrees.
#' @param lat Latitude in decimal degrees.
#' @param elev Sun elevation in degrees.
#' @param tz output time zone. Ignored if `date` has an associated time zone already
#' @param force_tz whether to convert output to timezone `tz`. Default `FALSE`.
#'
#' @return The moment of sunrise or sunset for the date set by `date`and time zone as specified
#' (by `date` and `tz`) or in UTC if not specified.
#'
#' @details
#' The day for which sunrise and sunset are calculated is given by the input date.
#' Sunrise and sunset are calculated relative to the moment of solar noon for that date,
#' i.e. the first sunrise before the moment of solar noon, and the first sunset after the
#' moment of solar noon. Therefore, depending on the timezone provided, it is possible that
#' the nearest sunrise prior to solar noon occurs a day earlier than the input date.
#' Similarly, sunset may occur a day later than the input date. See examples for details.
#'
#' The angular diameter of the sun is about 0.536 degrees,
#' therefore the moment of sunrise/sunset corresponds to half that elevation
#' at -0.268 degrees.
#'
#' This is a convenience function mapping to [crepuscule].
#'
#' Approximate astronomical formula are used, therefore the moment of
#' sunrise / sunset may be off by a few minutes
#'
#' If `force_tz` is `TRUE`, the output is converted to the timezone
#' set by `tz`
#'
#' @examples
#' # sunrise in the Netherlands
#' sunrise("2016-01-01", 5, 53)
#'
#' # sunset in the Netherlands
#' sunset("2016-01-01", 5, 53)
#'
#' # civil twilight in Ithaca, NY
#' sunrise("2016-01-01", -76.5, 42.4, elev = -6)
#'
#' # next sunset in South Dakota, USA
#' sunset("2016-11-15", -98, 45)
#'
#' # Beware that some days have two sunsets, or
#' # two sunrises! E.g. on 5 Oct (local timezone) at
#' # this location  sunset is actually on the 6 Oct
#' # in UTC time zone, i.e. the next day
#' sunset("2016-10-5", -98, 45)
#' # One day later, sunset is again on 6 Oct:
#' sunset("2016-10-6", -98, 45)
#'
#' # working in local time zones typically avoids such ambiguities:
#' sunset(lubridate::as_datetime("2016-06-05",tz="America/Chicago"), -98, 45)
#' sunset(lubridate::as_datetime("2016-06-06",tz="America/Chicago"), -98, 45)
#'
#' # use force_tz to force output to a specific time zone, by default UTC:
#' sunset(lubridate::as_datetime("2016-06-05",tz="America/Chicago"), -98, 45, force_tz=TRUE)
#' sunset(lubridate::as_datetime("2016-06-06",tz="America/Chicago"), -98, 45, force_tz=TRUE)
#'
#' # Also beware of jumps in sunrise and sunset date with longitude:
#' sunrise("2016-11-01", 100, 45)
#' sunrise("2016-11-01", 102, 45)
#'
#' @name sunrise_sunset
NULL

#' @rdname sunrise_sunset
#'
#' @export
sunrise <- function(date, lon, lat, elev = -0.268, tz = "UTC", force_tz = FALSE) {
  locations <- data.frame(lon = lon, lat = lat)
  locations <- SpatialPoints(locations, proj4string = CRS("+proj=longlat +datum=WGS84"))
  datetime <- as.POSIXct(date, tz = tz) # tz ignored if already set
  suntimes <- crepuscule(locations, datetime, solarDep = -elev, direction = "dawn", POSIXct.out = TRUE)
  if(force_tz) suntimes$time <- lubridate::as_datetime(suntimes$time, tz=tz)
  suntimes$time
}

#' @rdname sunrise_sunset
#'
#' @export
sunset <- function(date, lon, lat, elev = -0.268, tz = "UTC", force_tz = FALSE) {
  locations <- data.frame(lon = lon, lat = lat)
  locations <- SpatialPoints(locations, proj4string = CRS("+proj=longlat +datum=WGS84"))
  datetime <- as.POSIXct(date, tz = tz) # tz ignored if already set
  suntimes <- crepuscule(locations, datetime, solarDep = -elev, direction = "dusk", POSIXct.out = TRUE)
  if(force_tz) suntimes$time <- lubridate::as_datetime(suntimes$time, tz=tz)
  suntimes$time
}
