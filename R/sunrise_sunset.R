# function obtained via Hidde Leijnse, source unknown

#' Calculate sunrise or sunset for a time and place
#'
#' @param date Date inhereting from class \code{POSIXt} or a string
#' interpretable by \link[base]{as.Date}.
#' @param lon Longitude in decimal degrees.
#' @param lat Latitude in decimal degrees.
#' @param elev Sun elevation in degrees.
#' @param tz output time zone. Ignored if \code{date} has an associated time zone already
#'
#' @return The moment of sunrise or sunset in UTC time.
#'
#' @details The angular diameter of the sun is about 0.536 degrees,
#' therefore the moment of sunrise/sunset corresponds to half that elevation
#' at -0.268 degrees.
#' 
#' This is a convenience function mapping to \link[maptools]{crepuscule}.
#' 
#' Approximate astronomical formula are used, therefore the moment of
#' sunrise / sunset may be off by a few minutes
#'
#' @examples
#' # sunrise in the Netherlands
#' sunrise("2016-01-01", 5, 53)
#' # sunset in the Netherlands
#' sunset("2016-01-01", 5, 53)
#' # civil twilight in Ithaca, NY, today
#' sunrise(Sys.time(), -76.5, 42.4, elev = -6)
#'
#' @name sunrise_sunset
NULL

#' @rdname sunrise_sunset
#'
#' @export
sunrise <- function(date, lon, lat, elev = -0.268, tz="UTC") {
  locations <- data.frame(lon=lon, lat=lat)
  locations <- SpatialPoints(locations, proj4string = CRS("+proj=longlat +datum=WGS84"))
  dates <- as.POSIXct(date, tz = tz)
  suntimes=crepuscule(locations, dates, solarDep = -elev, direction = "dawn", POSIXct.out = TRUE)
  suntimes$time
}

#' @rdname sunrise_sunset
#'
#' @export
sunset <- function(date, lon, lat, elev = -0.268, tz="UTC") {
  locations <- data.frame(lon=lon, lat=lat)
  locations <- SpatialPoints(locations, proj4string = CRS("+proj=longlat +datum=WGS84"))
  dates <- as.POSIXct(date, tz = tz)
  suntimes=crepuscule(locations, dates, solarDep = -elev, direction = "dusk", POSIXct.out = TRUE)
  suntimes$time
}
