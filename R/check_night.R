#' Check if it is night at a given time and place
#'
#' Checks if it is night (`TRUE`/`FALSE`) for a combination of
#' latitude, longitude, date and sun elevation. When used on a bioRad object
#' (`pvol`, `vp`, `vpts`, `vpi`) this information is extracted from the
#' bioRad object directly.
#'
#' @param x `pvol`, `vp`, `vpts`, `vpi`,
#' or a date inheriting from class `POSIXct` or a string
#' interpretable by [as.POSIXct].
#' @param lon numeric. Longitude in decimal degrees.
#' @param lat numeric. Latitude in decimal degrees.
#' @param tz character. Time zone. Ignored when `date` already has an associated time zone
#' @param elev numeric. Sun elevation in degrees defining night time. May also be a numeric vector of
#' length two, with first element giving sunset elevation, and second element sunrise elevation.
#' @param offset numeric. Time duration in seconds by which to shift the start and end
#' of night time. May also be a numeric vector of length two, with first element added to moment
#' of sunset and second element added to moment of sunrise.
#' @param ... optional lat,lon arguments.
#'
#' @return `TRUE` when night, `FALSE` when day, `NA` if unknown
#' (either datetime or geographic location missing). For `vpts` a
#' vector of `TRUE`/`FALSE` values is returned.
#'
#' @export
#'
#' @details The angular diameter of the sun is about 0.536 degrees, therefore
#' the moment of sunrise/sunset corresponds to half that elevation at
#' -0.268 degrees.
#'
#' `check_night()` evaluates to `FALSE` when the sun has a higher
#' elevation than parameter `elev`, otherwise `TRUE`.
#'
#' Approximate astronomical formula are used, therefore the day/night
#' transition may be off by a few minutes.
#'
#' `offset` can be used to shift the moment of sunset and sunrise by a
#' temporal offset, for example, `offset=c(600,-900)` will assume nighttime
#' starts 600 seconds after sunset (as defined by `elev`) and stops 900 seconds before sunrise.
#'
#' @examples
#' # check if it is night at UTC midnight in the Netherlands on January 1st:
#' check_night("2016-01-01 00:00", 5, 53)
#'
#' # check on bioRad objects directly:
#' check_night(example_vp)
#'
#' check_night(example_vpts)
#'
#' # select nighttime profiles that are between 3 hours after sunset
#' # and 2 hours before sunrise:
#' index <- check_night(example_vpts, offset=c(3,-2)*3600)
#' example_vpts[index]
check_night <- function(x, ..., elev = -0.268, offset = 0) {
  UseMethod("check_night", x)
}

#' @rdname check_night
#'
#' @export
check_night.default <- function(x, lon, lat, ..., tz = "UTC", elev = -0.268, offset = 0) {
  # input checks
  assert_that(is.numeric(elev))
  assert_that(length(elev)<=2)
  assert_that(is.numeric(offset))
  assert_that(length(offset)<=2)
  assert_that(is.numeric(lon))
  assert_that(is.numeric(lat))
  assert_that(length(lat)==length(lon))
  #
  x <- as.POSIXct(x, tz = tz)
  #
  if(length(x)>1 & length(lon)==1){
    lon=rep(lon,length(x)) # to accomodate lon subsetting below
    lat=rep(lat,length(x)) # to accomodate lon subsetting below
  }
  #
  elev_sunset = ifelse(length(elev) == 2,elev[1],elev)
  elev_sunrise = ifelse(length(elev) == 2,elev[2],elev)
  # calculate sunrises
  trise <- sunrise(x, lon, lat, tz = tz, elev = elev_sunrise)
  # calculate sunsets
  tset <- sunset(x, lon, lat, tz = tz, elev = elev_sunset)

  # make sure the observation is always in between
  # two subsequent sunrise/sunset events:
  trise_ordered = trise
  tset_ordered = tset
  # sunrise -1 day
  idx <- which(x < tset  & trise > tset)
  if(length(idx)>0) trise_ordered[idx] = sunrise(x[idx]-24*3600, lon[idx], lat[idx], tz = tz, elev = elev_sunrise)
  # sunrise +1 day
  idx <- which(x > tset  & trise < tset)
  if(length(idx)>0) trise_ordered[idx] = sunrise(x[idx]+24*3600, lon[idx], lat[idx], tz = tz, elev = elev_sunrise)
  # sunset -1 day
  idx <- which(x < trise & trise < tset)
  if(length(idx)>0) tset_ordered[idx] = sunset(x[idx]-24*3600, lon[idx], lat[idx], tz = tz, elev = elev_sunset)
  # sunset +1 day
  idx <- which(x > trise & trise > tset)
  if(length(idx)>0) tset_ordered[idx] = sunset(x[idx]+24*3600, lon[idx], lat[idx], tz = tz, elev = elev_sunset)
  # store in original variable
  trise <- trise_ordered
  tset <- tset_ordered

  # add offset shifts
  offset_sunset = ifelse(length(offset) == 2,offset[1],offset)
  offset_sunrise = ifelse(length(offset) == 2,offset[2],offset)

  # prepare output
  output <- rep(NA, length(x))

  # cases trise < tset
  itsday <- (x > trise + offset_sunrise & x < tset + offset_sunset)
  output[trise < tset] <- itsday[trise < tset]
  # if order of sunrise and sunset switches due to offsets, daytime length is zero
  output[trise < tset  & (tset + offset_sunset) - (trise + offset_sunrise) <= 0] <- FALSE

  # cases trise >= tset
  itsday <- (x < tset + offset_sunset | x > trise + offset_sunrise)
  output[trise >= tset] <- itsday[trise >= tset]
  # if order of sunrise and sunset switches due to offsets, nighttime length is zero
  output[trise >= tset & (trise + offset_sunrise) - (tset + offset_sunset) <= 0] <- TRUE

  !output
}

#' @rdname check_night
#'
#' @export
check_night.vp <- function(x, ..., elev = -0.268, offset = 0) {
  stopifnot(inherits(x, "vp"))
  check_night(x$datetime, x$attributes$where$lon, x$attributes$where$lat,
    elev = elev, offset = offset
  )
}

#' @rdname check_night
#'
#' @export
check_night.list <- function(x, ..., elev = -0.268, offset = 0) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires list of vp objects as input")
  }
  sapply(x, check_night.vp, elev = elev, offset = offset)
}

#' @rdname check_night
#'
#' @export
check_night.vpts <- function(x, ..., elev = -0.268, offset = 0) {
  stopifnot(inherits(x, "vpts"))
  check_night(x$datetime, x$attributes$where$lon, x$attributes$where$lat,
    elev = elev, offset = offset
  )
}

#' @rdname check_night
#'
#' @export
check_night.vpi <- function(x, ..., elev = -0.268, offset = 0) {
  stopifnot(inherits(x, "vpi"))
  check_night(x$datetime, attributes(x)$lon, attributes(x)$lat,
              elev = elev, offset = offset
  )
}

#' @rdname check_night
#'
#' @export
check_night.pvol <- function(x, ..., elev = -0.268, offset = 0) {
  stopifnot(inherits(x, "pvol"))
  check_night(x$datetime, x$geo$lon, x$geo$lat, elev = elev, offset = offset)
}
