#' Time selection in a time series of vertical profiles ('vpts')
#'
#' @param x A \code{vpts} object.
#' @param min POSIXct value. Minimum datetime to be included
#' @param max POSIXct value. Maximum datetime to be included
#' @return An object of class '\link[=summary.vpts]{vpts}'.
#'
#' @export
#' @details returns profiles for which min <= timestamp profile < max.
#' @examples
#' # let us use this example vertical profile time series:
#' example_vpts
#' # select profiles later than 02-Sep-2016
#' filter_datetime(example_vpts, min=as.POSIXct("2016-09-02"))
filter_datetime = function(x, min, max){
  assert_that(is.vpts(x))
  if(!missing(min)){
    assert_that(is.time(min))
    assert_that(length(min) == 1)

  }
  if(!missing(max)){
    assert_that(is.time(max))
    assert_that(length(max) == 1)
  }
  if(missing(min) & missing(max)) return(x)
  if(missing(min) & !missing(max)) idx=x$datetime<max
  if(!missing(min) & missing(max)) idx=x$datetime>=min
  if(!missing(min) & !missing(max)) idx=(x$datetime>=min & x$datetime<max)
  no_profiles=length(which(idx))
  if(no_profiles==0){
    warning("no profiles passing datetime filter, returning empty vpts object")
    suppressWarnings(return(x[idx]))
  }
  if(no_profiles==1){
    warning("only a single profile passed datetime filter")
    return(x[idx])
  }
  return(x[idx])
}
