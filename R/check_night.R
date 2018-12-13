#' Check if it is night at a given time and place
#'
#' Checks if it is night (\code{TRUE}/\code{FALSE}) for a combination of
#' latitude, longitude, date and sun elevation. When used on a bioRad object
#' (\code{pvol}, \code{vp}, \code{vpts}) this information is extracted from the
#' bioRad object directly.
#'
#' @param x \code{pvol}, \code{vp} or \code{vpts},
#' or a date inheriting from class \code{POSIXct} or a string
#' interpretable by \link[base]{as.POSIXct}.
#' @param lon numeric. Longitude in decimal degrees.
#' @param lat numeric. Latitude in decimal degrees.
#' @param tz character. Time zone. Ignored when \code{date} already has an associated time zone
#' @param elev numeric. Sun elevation in degrees.
#' @param ... optional lat,lon arguments.
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
#' check_night("2016-01-01 00:00", 5, 53)
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
check_night.default <- function(x, lon, lat, ..., tz = "UTC", elev = -0.268) {
  x <- as.POSIXct(x, tz = tz)
  # calculate sunrises
  trise <- sunrise(x, lon, lat, tz = tz, elev = elev)
  # calculate sunsets
  tset <- sunset(x, lon, lat, tz = tz, elev = elev)
  # for returned rise times on a different day, recalculate for the current day
  dt <- as.numeric(difftime(as.Date(trise), as.Date(x), units = "days"))
  change <- which(dt != 0)
  if (length(change) > 0) {
    trise[change] <- sunrise(x[change] - dt[change] * 24 * 3600, lon, lat,
      tz = tz, elev = elev
    )
  }

  dt <- as.numeric(difftime(as.Date(tset), as.Date(x), units = "days"))
  change <- which(dt != 0)
  if (length(change) > 0) {
    tset[change] <- sunset(x[change] - dt[change] * 24 * 3600, lon, lat,
      tz = tz, elev = elev
    )
  }

  # prepare output
  output <- rep(NA, length(x))
  itsday <- (x > trise & x < tset)
  output[trise < tset] <- itsday[trise < tset]
  itsday <- (x < tset | x > trise)
  output[trise >= tset] <- itsday[trise >= tset]
  !output
}

#' @rdname check_night
#'
#' @export
check_night.vp <- function(x, ..., elev = -0.268) {
  stopifnot(inherits(x, "vp"))
  check_night(x$datetime, x$attributes$where$lon, x$attributes$where$lat,
    elev = elev
  )
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
  check_night(x$datetime, x$attributes$where$lon, x$attributes$where$lat,
    elev = elev
  )
}

#' @rdname check_night
#'
#' @export
check_night.pvol <- function(x, ..., elev = -0.268) {
  stopifnot(inherits(x, "pvol"))
  check_night(x$datetime, x$geo$lon, x$geo$lat, elev = elev)
}
