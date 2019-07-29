#' Time selection in a time series of vertical profiles ('vpts')
#'
#' @param x A \code{vpts} object.
#' @param min Minimum datetime to be included. POSIXct value or charater string convertable to POSIXct.
#' @param max Maximum datetime to be included. POSIXct value or charater string convertable to POSIXct.
#' @param nearest If specified, \code{min} and \code{max} are ignored and the profile nearest to the
#' specified datetime is returned. POSIXct value or charater string convertable to POSIXct.
#' @return An object of class '\link[=summary.vpts]{vpts}', or an object of class '\link[=summary.vp]{vp}'
#' if argument \code{nearest} is specified.
#' @export
#' @details returns profiles for which min <= timestamp profile < max.
#' @examples
#' # let us use this example vertical profile time series:
#' example_vpts
#' # select profiles later than 02-Sep-2016
#' filter_vpts(example_vpts, min = as.POSIXct("2016-09-02"))
filter_vpts <- function(x, min, max, nearest) {
  assert_that(is.vpts(x))
  errorf <- function(e) {
    min
  }
  if (!missing(min)) {
    if (is.string(min)) {
      min <- tryCatch(as.POSIXct(min), errorf = function(e) {
        min
      })
    }
    assert_that(is.time(min))
    assert_that(length(min) == 1)
  }
  if (!missing(max)) {
    if (is.string(max)) {
      max <- tryCatch(as.POSIXct(max), errorf = function(e) {
        max
      })
    }
    assert_that(is.time(max))
    assert_that(length(max) == 1)
  }
  if (!missing(nearest)) {
    if (is.string(nearest)) {
      nearest <- tryCatch(as.POSIXct(nearest), errorf = function(e) {
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
    warning("only a single profile passed datetime filter")
    return(x[idx])
  }
  return(x[idx])
}
