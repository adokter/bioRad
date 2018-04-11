#' Calculate whether it is night at a geographic location and time
#' @inheritParams suntime
#' @export
#' @return TRUE when night, FALSE when day
#' @details The angular diameter of the sun is about 0.536 degrees, therefore the moment
#' of sunrise/sunset corresponds to half that elevation at -0.268 degrees.
#'
#' day evaluates to true when the sun has a higher elevation than parameter elev, otherwise to false
#'
#' Approximate astronomical formula are used, therefore the day/night transition may
#' be off by a few minutes
#' @examples
#' # it's day in the Netherlands at UTC noon on January first:
#' night(5,53,"2016-01-01 12:00")
#'
#' @export
night=function(lon,lat,date,elev=-0.268){
  trise=suntime(lon,lat,date,elev,rise=T)
  tset=suntime(lon,lat,date,elev,rise=F)
  output=rep(NA,length(date))
  itsday=(date>trise & date<tset)
  output[trise<tset]=itsday[trise<tset]
  itsday=(date<tset | date>trise)
  output[trise>=tset]=itsday[trise>=tset]
  !output
}

#' Test a bioRad object for night time
#'
#' Test a bioRad object for day time. Dispatches to the logical inverse of \link{night}.
#' @inheritParams night
#' @param x An object of class \code{vp},\code{vplist} or \code{vpts}.
#' @export
#' @return TRUE when day, FALSE when night, NA if unknown (either datetime or geographic location missing). For objects of class vpts an atomic logical vector
#' @examples
#' day(VP)
day <- function (x, elev=-0.268) UseMethod("day", x)

#' @rdname day
#' @export
day.vp <- function(x,elev=-0.268) {
  stopifnot(inherits(x,"vp"))
  !night(x$attributes$where$lon,x$attributes$where$lat,x$datetime,elev=elev)
}

#' @rdname day
#' @export
day.vplist <- function(x,elev=-0.268) {
  stopifnot(inherits(x,"vplist"))
  sapply(x,day.vp,elev=elev)
}

#' @rdname day
#' @export
day.vpts <- function(x,elev=-0.268) {
  stopifnot(inherits(x,"vpts"))
  !night(x$attributes$where$lon,x$attributes$where$lat,x$dates,elev=elev)
}

#' @rdname day
#' @export
day.pvol <- function(x,elev=-0.268) {
  stopifnot(inherits(x,"pvol"))
  !night(x$geo$lon,x$geo$lat,x$datetime,elev=elev)
}
