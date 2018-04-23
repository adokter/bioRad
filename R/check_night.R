#' Check if it is night at a given time and place
#'
#' Checks if it is night (\code{TRUE}/\code{FALSE}) for a combination of
#' latitude, longitude, date and sun elevation. When used on a bioRad object
#' (\code{pvol}, \code{vp}, \code{vpts}) this information is extracted from the
#' bioRad object directly.
#'
#' @param x A \code{numeric}, \code{pvol}, \code{vp} or \code{vpts}.
#' @param lon numeric. Longitude in decimal degrees.
#' @param lat numeric. Latitude in decimal degrees.
#' @param date Date. Date inheriting from class \code{POSIXt} or a string
#' interpretable by \link[base]{as.Date}.
#' @param elev numeric. Sun elevation in degrees.
#' @param ... A bioRad object of lat/lon combination.
#'
#' @return \code{TRUE} when night, \code{FALSE} when day, \code{NA} if unknown
#' (either datetime or geographic location missing). For \code{vpts} a
#' vector of \code{TRUE}/\code{FALSE} values is returned.
#'
#' @export
#'
#' @details The angular diameter of the sun is about 0.536 degrees, therefore
#' the moment of sunrise/sunset corresponds to half that elevation at
#' -0.268 degrees.
#'
#' \code{check_night()} evaluates to \code{FALSE} when the sun has a higher
#' elevation than parameter \code{elev}, otherwise \code{TRUE}.
#'
#' Approximate astronomical formula are used, therefore the day/night
#' transition may be off by a few minutes.
#'
#' @examples
#' # check if it is night at UTC midnight in the Netherlands on January 1st:
#' check_night(5, 53, "2016-01-01 00:00")
#'
#' # check on bioRad objects directly:
#' check_night(example_vp)
#' check_night(example_vpts)
check_night <- function(x, ..., elev = -0.268) {
  UseMethod("check_night", x)
}

#' @rdname check_night
#'
#' @export
check_night.default <- function(lon, lat, date, elev = -0.268) {
  trise <- sunrise(lon, lat, date, elev)
  tset <- sunset(lon, lat, date, elev)
  output <- rep(NA, length(date))
  itsday <- (date > trise & date < tset)
  output[trise < tset] <- itsday[trise < tset]
  itsday <- (date < tset | date > trise)
  output[trise >= tset] <- itsday[trise >= tset]
  !output
}

#' @rdname check_night
#'
#' @export
check_night.vp <- function(x, ..., elev = -0.268) {
  stopifnot(inherits(x, "vp"))
  check_night(x$attributes$where$lon, x$attributes$where$lat,
              x$datetime, elev = elev)
}

#' @rdname check_night
#'
#' @export
check_night.list <- function(x, ..., elev = -0.268) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires list of vp objects as input")
  }
  sapply(x, check_night.vp, elev = elev)
}

#' @rdname check_night
#'
#' @export
check_night.vpts <- function(x, ..., elev = -0.268) {
  stopifnot(inherits(x, "vpts"))
  check_night(x$attributes$where$lon, x$attributes$where$lat,
         x$dates, elev = elev)
}

#' @rdname check_night
#'
#' @export
check_night.pvol <- function(x, ..., elev = -0.268) {
  stopifnot(inherits(x, "pvol"))
  check_night(x$geo$lon, x$geo$lat, x$datetime, elev = elev)
}
