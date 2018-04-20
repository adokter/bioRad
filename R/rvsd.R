#' Get threshold of the VVP-retrieved radial velocity standard deviation
#'
#' Gives the current threshold in VVP-retrieved radial velocity standard deviation in m/s.
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @export
#' @return threshold for \code{sd_vvp} in m/s.
#' @examples
#' # extract threshold for a single vertical profile:
#' rvsd(example_vp)
rvsd <- function (x) UseMethod("rvsd", x)

#' @describeIn rvsd threshold in VVP-retrieved radial velocity standard deviation of a vertical profile
#' @export
rvsd.vp <- function (x){
  stopifnot(inherits(x,"vp"))
  x$attributes$how$sd_vvp_thresh
}

#' @describeIn rvsd threshold in VVP-retrieved radial velocity standard deviation of a list of vertical profiles
#' @export
rvsd.vplist <- function (x){
  stopifnot(inherits(x,"vplist"))
  output=sapply(x,`rvsd.vp`)
  output
}

#' @describeIn rvsd threshold in VVP-retrieved radial velocity standard deviation of a time series of vertical profiles
#' @export
rvsd.vpts <- function (x){
  stopifnot(inherits(x,"vpts"))
  x$attributes$how$sd_vvp_thresh
}

#' Set threshold of the VVP-retrieved radial velocity standard deviation
#'
#' Sets the threshold in \code{sd_vvp}. Altitude layers with \code{sd_vvp} below this threshold are
#' assumed to have an aerial density of zero individuals. This method updates the migration densities in \code{x$data$dens}
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @param value the value to assign
#' @export
#' @examples
#' # change threshold for a single vertical profile:
#' rvsd(example_vp) <- 2
`rvsd<-` <- function (x, value) UseMethod("rvsd<-", x)

#' @rdname rvsd-set
#' @method rvsd<- vp

#' @export
`rvsd<-.vp` <- function(x,value){
  stopifnot(inherits(x,"vp"))
  x$attributes$how$sd_vvp_thresh=value
  if(is.numeric(x$attributes$how$rcs_bird)){
    x$data$dens=x$data$eta/x$attributes$how$rcs_bird
    x$data$dens[x$data$sd_vvp<value]=0
  }
  else{
    warning("radar cross section not set, defaulting to 11 cm^2 ...")
    x$data$dens=x$data$eta/11
    x$attributes$how$rcs_bird=11
    x$data$dens[x$data$sd_vvp<value]=0
  }
  x
}

#' @rdname rvsd-set
#' @export
`rvsd<-.vplist` <- function(x,value){
  stopifnot(inherits(x,"vplist"))
  output=lapply(x,`rvsd<-.vp`,value=value)
  class(output)=c("vplist","list")
  output
}

#' @rdname rvsd-set
#' @export
`rvsd<-.vpts` <- function(x,value){
  stopifnot(inherits(x,"vpts"))
  x$attributes$how$sd_vvp_thresh=value
  if(is.numeric(x$attributes$how$rcs_bird)){
    x$data$dens=x$data$eta/x$attributes$how$rcs_bird
    x$data$dens[x$data$sd_vvp<value]=0
  }
  else{
    warning("radar cross section not set, defaulting to 11 cm^2 ...")
    x$data$dens=x$data$eta/11
    x$attributes$how$rcs_bird=11
    x$data$dens[x$data$sd_vvp<value]=0
  }
  x
}
