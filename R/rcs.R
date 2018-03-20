#' Radar cross section
#'
#' Gives the currently assumed radar cross section in cm^2.
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @export
#' @return a radar cross section in cm^2
#' @examples
#' # extract RCS for a single vertical profile:
#' rcs(VP)
rcs <- function (x) UseMethod("rcs", x)

#' @describeIn rcs radar cross section of a vertical profile
#' @export
rcs.vp <- function (x){
  stopifnot(inherits(x,"vp"))
  x$attributes$how$rcs_bird
}

#' @describeIn rcs radar cross sections for a list of vertical profiles
#' @export
rcs.vplist <- function (x){
  stopifnot(inherits(x,"vplist"))
  output=sapply(x,`rcs.vp`)
  output
}

#' @describeIn rcs radar cross section of a time series of vertical profile
#' @export
rcs.vpts <- function (x){
  stopifnot(inherits(x,"vpts"))
  x$attributes$how$rcs_bird
}

#' @describeIn rcs radar cross section of a time series of vertically integrated vertical profile(s)
#' @export
rcs.vivp <- function (x){
  stopifnot(inherits(x,"vivp"))
  attributes(x)$rcs
}

#' Set radar cross section
#'
#' Sets the assumed radar cross section in cm^2. This method also updates the migration densities in \code{x$data$dens}
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @param value the cross section value to assign
#' @export
#' @examples
#' # change RCS for a single vertical profile:
#' rcs(VP)<-20
`rcs<-` <- function (x, value) UseMethod("rcs<-", x)

#' @rdname rcs-set

#' @export
`rcs<-.vp` <- function(x,value){
  stopifnot(inherits(x,"vp"))
  x$attributes$how$rcs_bird=value
  x$data$dens=x$data$eta/value
  if(is.numeric(x$attributes$how$sd_vvp_thresh)){
    x$data$dens[x$data$sd_vvp<x$attributes$how$sd_vvp_thresh]=0
  }
  else{
    warning("threshold for sd_vvp not set, defaulting to 2 m/s")
    x$attributes$how$sd_vvp_thresh=2
    x$data$dens[x$data$sd_vvp<2]=0
  }
  x
}

#' @rdname rcs-set
#' @export
`rcs<-.vplist` <- function(x,value){
  stopifnot(inherits(x,"vplist"))
  output=lapply(x,`rcs<-.vp`,value=value)
  class(output)=c("vplist","list")
  output
}

#' @rdname rcs-set
#' @export
`rcs<-.vpts` <- function(x,value){
  stopifnot(inherits(x,"vpts"))
  x$attributes$how$rcs_bird=value
  x$data$dens=x$data$eta/value
  if(is.numeric(x$attributes$how$sd_vvp_thresh)){
    x$data$dens[x$data$sd_vvp<x$attributes$how$sd_vvp_thresh]=0
  }
  else{
    warning("threshold for sd_vvp not set, defaulting to 2 m/s")
    x$attributes$how$sd_vvp_thresh=2
    x$data$dens[x$data$sd_vvp<2]=0
  }
  x
}

#' @rdname rcs-set
#' @export
`rcs<-.vivp` <- function(x,value){
  stopifnot(inherits(x,"vivp"))
  attributes(x)$rcs=value
  x$mtr=x$rtr/value
  x$vid=x$vir/value
  x
}
