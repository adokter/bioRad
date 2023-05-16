#' Time and night/day selection in a time series of vertical profiles ('vpts')
#'
#' @param x A `vpts` object.
#' @inheritParams check_night
#' @param min Minimum datetime to be included. POSIXct value or character string convertible to POSIXct.
#' @param max Maximum datetime to be included. POSIXct value or character string convertible to POSIXct.
#' @param nearest If specified, `min` and `max` are ignored and the profile nearest to the
#' specified datetime is returned that matches the day/night selection criteria.
#' POSIXct value or character string convertible to POSIXct.
#' @param night When TRUE select only night time profiles, when FALSE select only day time profiles,
#' as classified by [check_night].
#' @param offset numeric. Time duration in seconds by which to shift the start and end
#' of night time. May also be a numeric vector of length two, with first element added to moment
#' of sunset and second element added to moment of sunrise. See [check_night] for details.
#' @return An object of class '[vpts][summary.vpts]', or an object of class '[vp][summary.vp]'
#' if argument `nearest` is specified.
#' @export
#' @details Returns profiles for which min <= timestamp profile < max. Selection for night and day
#' occurs by [check_night].
#' @examples
#' # load example vertical profile time series:
#' data(example_vpts)
#' example_vpts
#'
#' # select profiles later than 02-Sep-2016
#' filter_vpts(example_vpts, min = "2016-09-02")
#'
#' # select the profile nearest to 2016-09-01 03:00 UTC:
#' filter_vpts(example_vpts, nearest = "2016-09-01 03:00")
#'
#' # select profiles between than 1 and 3 UTC on 02-Sep-2016:
#' filter_vpts(example_vpts, min = "2016-09-02 01:00", max = "2016-09-02 03:00")
#'
#' # select day time profiles (day time periods from sunrise to sunset)
#' filter_vpts(example_vpts, night=FALSE)
#'
#' # select night time profiles, with nights starting starting and ending at civil twilight
#' # (when the sun is 6 degrees below the horizon):
#' filter_vpts(example_vpts, night=TRUE, elev = -6)
#'
#' # select night time profiles from 3 hours after sunset to 2 hours before sunrise
#' filter_vpts(example_vpts, night=TRUE, offset=c(3,-2)*3600)
#'
filter_vpts <- function(x, min, max, nearest, night, elev = -0.268, offset = 0) {
  assert_that(is.vpts(x))
  errorf <- function(e) {
    min
  }

  if(!missing(night)) assert_that(is.logical(night))

  if (!missing(min)) {
    if (is.string(min)) {
      min <- tryCatch(as.POSIXct(min, tz = "UTC"), errorf = function(e) {
        min
      })
    }
    assert_that(is.time(min))
    assert_that(length(min) == 1)
  }
  if (!missing(max)) {
    if (is.string(max)) {
      max <- tryCatch(as.POSIXct(max, tz = "UTC"), errorf = function(e) {
        max
      })
    }
    assert_that(is.time(max))
    assert_that(length(max) == 1)
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
    if (is.string(nearest)) {
      nearest <- tryCatch(as.POSIXct(nearest, tz = "UTC"), errorf = function(e) {
        nearest
      })
    }
    assert_that(is.time(nearest))
    assert_that(length(nearest) == 1)
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
