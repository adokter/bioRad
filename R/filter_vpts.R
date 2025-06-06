#' Filter a time series of vertical profiles ('vpts').
#'
#' Filter a time series of vertical profiles ('vpts') by a start and end time,
#' for night or day time, or extract profile nearest to provided timestamp.
#' Use argument night = TRUE to select only time stamps between sunset and sunrise,
#' or night = FALSE to select daytime (sunrise to sunset). Selection for night and day uses
#' [check_night()].
#'
#' @param x A `vpts` object.
#' @inheritParams check_night
#' @param min POSIXct date or character. Minimum datetime to be included.
#' @param max POSIXct date or character. Datetime up to this maximum included.
#' @param nearest POSIXct date or character. If specified, `min` and `max` are
#'   ignored and the profile (`vp`) nearest to the specified datetime is
#'   returned that matches the day/night selection criteria.
#' @param night When `TRUE` selects only nighttime profiles, when `FALSE`
#'   selects only daytime profiles, as classified by [check_night()].
#' @param offset Numeric (vector). Time duration in seconds by which to shift
#'   the start and end of nighttime. May also be a numeric vector of length two,
#'   with first element added to moment of sunset and second element added to
#'   moment of sunrise. See [check_night()] for details.
#'
#' @return A `vpts` object, or a `vp` object when `nearest` is specified.
#'
#' @export
#' @details Returns profiles for which min <= timestamp profile < max. Selection for night and day
#' occurs by [check_night].
#' @seealso
#' * [summary.vpts()]
#' * [check_night()]
#'
#' @examples
#' # Select profiles later than 02 Sep 2016
#'
#' # Select the profile nearest to 2016-09-01 03:00 UTC
#' filter_vpts(example_vpts, nearest = "2016-09-01 03:00")
#'
#' # Select profiles between than 01:00 and 03:00 UTC on 02 Sep 2016
#' filter_vpts(example_vpts, min = "2016-09-02 01:00", max = "2016-09-02 03:00")
#'
#' # Select daytime profiles (i.e. profiles between sunrise and sunset)
#' filter_vpts(example_vpts, night = FALSE)
#'
#' # Select nighttime profiles, with nights starting and ending at
#' # civil twilight (when the sun is 6 degrees below the horizon)
#' filter_vpts(example_vpts, night = TRUE, elev = -6)
#'
#' # Select nighttime profiles from 3h after sunset to 2h before sunrise
#' filter_vpts(example_vpts, night = TRUE, offset = c(3, -2)*3600)
#'
filter_vpts <- function(x, min, max, nearest, night, elev = -0.268, offset = 0) {
  assertthat::assert_that(is.vpts(x),
                          msg = "`x` is not a `vpts` object")
  errorf <- function(e) {
    min
  }

  if(!missing(night)) assertthat::assert_that(assertthat::is.flag(night))

  if (!missing(min)) {
    if (assertthat::is.string(min)) {
      min <- tryCatch(as.POSIXct(min, tz = "UTC"), errorf = function(e) {
        min
      })
    }
    assertthat::assert_that(assertthat::is.time(min))
    assertthat::assert_that(length(min) == 1)
  }
  if (!missing(max)) {
    if (assertthat::is.string(max)) {
      max <- tryCatch(as.POSIXct(max, tz = "UTC"), errorf = function(e) {
        max
      })
    }
    assertthat::assert_that(assertthat::is.time(max))
    assertthat::assert_that(length(max) == 1)
  }

  # filter by day/night if requested
  if(!missing(night)){
    idx <- check_night(x, elev=elev, offset=offset)
    if(!night) idx <- !idx
    if(!(TRUE %in% idx)){
      warning("no profiles pass filtering by day/night, returning empty vpts object")
      suppressWarnings(return(x[idx]))
    }
    # keep profiles passing day/night filter
    x <- x[idx]
  }

  if (!missing(nearest)) {
    if (assertthat::is.string(nearest)) {
      nearest <- tryCatch(as.POSIXct(nearest, tz = "UTC"), errorf = function(e) {
        nearest
      })
    }
    assertthat::assert_that(assertthat::is.time(nearest))
    assertthat::assert_that(length(nearest) == 1)
    idx <- which.min(abs(difftime(x$datetime, nearest)))
    return(x[idx])
  }
  if (missing(min) & missing(max)) {
    return(x)
  }
  if (missing(min) & !missing(max)) idx <- x$datetime < max
  if (!missing(min) & missing(max)) idx <- x$datetime >= min
  if (!missing(min) & !missing(max)) idx <- (x$datetime >= min & x$datetime < max)
  no_profiles <- length(which(idx))
  if (no_profiles == 0) {
    warning("no profiles passing datetime filter, returning empty vpts object")
    suppressWarnings(return(x[idx]))
  }
  if (no_profiles == 1) {
    warning("only a single profile passed datetime filter, returning a vp object")
    return(x[idx])
  }
  return(x[idx])
}
