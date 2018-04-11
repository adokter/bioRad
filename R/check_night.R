
#' Calculate whether it is night at a geographic location and time
#'
#' The night check is based on the combination of lat, lon, date and the sun
#' elevation. When testing a bioRad object for day/night time, this information
#' is extracted from the bioRad data types directly
#'
#' @param x An object of class \code{numeric}, \code{vp},
#' \code{vplist} or \code{vpts}.
#' @param lon longitude in decimal degrees
#' @param lat latitude in decimal degrees
#' @param date date inhereting from class "\code{POSIXt}" or a string
#' interpretable by \link[base]{as.Date}
#' @param elev sun elevation in degrees
#'
#' @details The angular diameter of the sun is about 0.536 degrees, therefore
#' the moment of sunrise/sunset corresponds to half that elevation at
#' -0.268 degrees.
#'
#' check_night evaluates to FALSE when the sun has a higher elevation than
#' parameter elev, otherwise to TRUE
#'
#' Approximate astronomical formula are used, therefore the day/night
#' transition may be off by a few minutes
#'
#' @export
#' @return TRUE when night, FALSE when day, NA if unknown (either datetime or
#' geographic location missing). For objects of class vpts an atomic logical
#' vector
#'
#' @examples
#' # it's day in the Netherlands at UTC noon on January first:
#' check_night(5,53,"2016-01-01 12:00")
#' # applied on the bioRad object classes
#' check_night(VP)
#' check_night(VPTS)
check_night <- function(x, ..., elev=-0.268) UseMethod("check_night", x)

#' @rdname check_night
#' @export
check_night.default <- function(lon, lat, date, elev = -0.268){
  trise <- suntime(lon, lat, date, elev, rise = TRUE)
  tset <- suntime(lon, lat, date, elev, rise = FALSE)
  output <- rep(NA, length(date))
  itsday <- (date > trise & date < tset)
  output[trise < tset] <- itsday[trise < tset]
  itsday <- (date < tset | date > trise)
  output[trise >= tset] <- itsday[trise >= tset]
  !output
}

#' @rdname check_night
#' @export
check_night.vp <- function(x, elev = -0.268) {
  stopifnot(inherits(x, "vp"))
  check_night(x$attributes$where$lon, x$attributes$where$lat,
              x$datetime, elev = elev)
}

#' @rdname check_night
#' @export
check_night.vplist <- function(x, elev = -0.268) {
  stopifnot(inherits(x, "vplist"))
  sapply(x, check_night.vp, elev = elev)
}

#' @rdname check_night
#' @export
check_night.vpts <- function(x, elev = -0.268) {
  stopifnot(inherits(x, "vpts"))
  check_night(x$attributes$where$lon, x$attributes$where$lat,
         x$dates, elev = elev)
}

#' @rdname check_night
#' @export
check_night.pvol <- function(x, elev = -0.268) {
  stopifnot(inherits(x, "pvol"))
  check_night(x$geo$lon, x$geo$lat, x$datetime, elev = elev)
}
