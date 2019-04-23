#' Get threshold of the VVP-retrieved radial velocity standard deviation
#'
#' Gives the current threshold in VVP-retrieved radial velocity standard
#' deviation in m/s.
#'
#' @param x A \code{vp}, list of \code{vp} or \code{vpts} object.
#'
#' @return threshold for \code{sd_vvp} in m/s.
#'
#' @details See also \link{sd_vvp_threshold<-} for setting an objects radial
#' velocity standard deviation.

#'
#' @export
#'
#' @examples
#' # retrieve threshold for a single vertical profile:
#' sd_vvp_threshold(example_vp)
#' # retrieve threshold for a vertical profile time series:
#' sd_vvp_threshold(example_vpts)
#' # change or set the threshold for a single vertical profile:
#' sd_vvp_threshold(example_vp) = 2
#' # change or set the threshold for a vertical profile time series:
#' sd_vvp_threshold(example_vp) = 2
sd_vvp_threshold <- function(x) {
  UseMethod("sd_vvp_threshold", x)
}

#' @describeIn sd_vvp_threshold threshold in VVP-retrieved radial velocity standard
#' deviation of a vertical profile
#'
#' @export
sd_vvp_threshold.vp <- function(x) {
  stopifnot(inherits(x, "vp"))
  x$attributes$how$sd_vvp_thresh
}

#' @describeIn sd_vvp_threshold threshold in VVP-retrieved radial velocity standard
#' deviation of a list of vertical profiles
#'
#' @export
sd_vvp_threshold.list <- function(x) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires list of vp objects as input")
  }
  output <- sapply(x, `sd_vvp_threshold.vp`)
  output
}

#' @describeIn sd_vvp_threshold threshold in VVP-retrieved radial velocity standard
#' deviation of a time series of vertical profiles
#' @export
sd_vvp_threshold.vpts <- function(x) {
  stopifnot(inherits(x, "vpts"))
  x$attributes$how$sd_vvp_thresh
}

#' Set threshold of the VVP-retrieved radial velocity standard deviation
#'
#' Sets the threshold for \code{sd_vvp}. Altitude layers with \code{sd_vvp}
#' below this threshold are assumed to have an aerial density of zero
#' individuals. This method updates the migration densities
#' in \code{x$data$dens}
#'
#' @param x a \code{vp}, list of \code{vp} or \code{vpts} object
#' @param value the value to assign
#'
#' @export
#'
#' @details See also \link{sd_vvp_threshold} for retrieving an objects radial
#' velocity standard deviation.
#'
#' @examples
#' # change or set the threshold for a single vertical profile:
#' sd_vvp_threshold(example_vp) <- 2
#' # change or set the threshold for a vertical profile time series:
#' sd_vvp_threshold(example_vp) <- 2
#' # retrieve threshold for a single vertical profile:
#' sd_vvp_threshold(example_vp)
#' # retrieve threshold for a vertical profile time series:
#' sd_vvp_threshold(example_vpts)
`sd_vvp_threshold<-` <- function(x, value) {
  UseMethod("sd_vvp_threshold<-", x)
}

#' @rdname sd_vvp_threshold-set
#'
#' @method sd_vvp_threshold<- vp
#'
#' @export
`sd_vvp_threshold<-.vp` <- function(x, value) {
  stopifnot(inherits(x, "vp"))
  x$attributes$how$sd_vvp_thresh <- value
  if (is.numeric(x$attributes$how$rcs_bird)) {
    x$data$dens <- x$data$eta / x$attributes$how$rcs_bird
    x$data$dens[x$data$sd_vvp < value] <- 0
  } else {
    warning("Radar cross section not set, defaulting to 11 cm^2 ...")
    x$data$dens <- x$data$eta / 11
    x$attributes$how$rcs_bird <- 11
    x$data$dens[x$data$sd_vvp < value] <- 0
  }
  x
}

#' @rdname sd_vvp_threshold-set
#'
#' @export
`sd_vvp_threshold<-.list` <- function(x, value) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires list of vp objects as input")
  }
  output <- lapply(x, `sd_vvp_threshold<-.vp`, value = value)
  class(output) <- c("list")
  output
}

#' @rdname sd_vvp_threshold-set
#'
#' @export
`sd_vvp_threshold<-.vpts` <- function(x, value) {
  stopifnot(inherits(x, "vpts"))
  x$attributes$how$sd_vvp_thresh <- value
  if (is.numeric(x$attributes$how$rcs_bird)) {
    x$data$dens <- x$data$eta / x$attributes$how$rcs_bird
    x$data$dens[x$data$sd_vvp < value] <- 0
  } else {
    warning("Radar cross section not set, defaulting to 11 cm^2 ...")
    x$data$dens <- x$data$eta / 11
    x$attributes$how$rcs_bird <- 11
    x$data$dens[x$data$sd_vvp < value] <- 0
  }
  x
}
