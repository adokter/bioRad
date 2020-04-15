#' Get threshold of the VVP-retrieved radial velocity standard deviation
#'
#' Gives the current threshold in VVP-retrieved radial velocity standard
#' deviation in m/s.
#'
#' @param x A `vp`, list of `vp` or `vpts` object.
#'
#' @return Threshold for `sd_vvp`` in m/s.
#'
#' @seealso [`sd_vvp_threshold()<-`] for setting an objects radial velocity
#'   standard deviation.
#'
#' @export
#'
#' @examples
#' # load example data:
#' data(example_vp)
#' data(example_vpts)
#'
#' # retrieve threshold for a single vertical profile:
#' sd_vvp_threshold(example_vp)
#'
#' # retrieve threshold for a vertical profile time series:
#' sd_vvp_threshold(example_vpts)
#'
#' # change or set the threshold for a single vertical profile:
#' sd_vvp_threshold(example_vp) <- 2
#'
#' # change or set the threshold for a vertical profile time series:
#' sd_vvp_threshold(example_vp) <- 2
sd_vvp_threshold <- function(x) {
  UseMethod("sd_vvp_threshold", x)
}

#' @rdname sd_vvp_threshold
#'
#' @export
sd_vvp_threshold.vp <- function(x) {
  stopifnot(inherits(x, "vp"))
  x$attributes$how$sd_vvp_thresh
}

#' @rdname sd_vvp_threshold
#'
#' @export
sd_vvp_threshold.list <- function(x) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("Input must be list of vp objects.")
  }
  output <- sapply(x, `sd_vvp_threshold.vp`)
  output
}

#' @rdname sd_vvp_threshold
#'
#' @export
sd_vvp_threshold.vpts <- function(x) {
  stopifnot(inherits(x, "vpts"))
  x$attributes$how$sd_vvp_thresh
}

#' Set threshold of the VVP-retrieved radial velocity standard deviation
#'
#' Sets the threshold for `sd_vvp`. Altitude layers with `sd_vvp`
#' below this threshold are assumed to have an aerial density of zero
#' individuals. This method updates the migration densities
#' in `x$data$dens`.
#'
#' @param x a `vp`, list of `vp` or `vpts` object
#' @param value The value to assign.
#'
#' @export
#'
#' @seealso [`sd_vvp_threshold()`] for retrieving an objects radial
#' velocity standard deviation.
#'
#' @examples
#' # load example data:
#' data(example_vp)
#' data(example_vpts)
#'
#' # change or set the threshold for a single vertical profile:
#' sd_vvp_threshold(example_vp) <- 2
#'
#' # change or set the threshold for a vertical profile time series:
#' sd_vvp_threshold(example_vp) <- 2
#'
#' # retrieve threshold for a single vertical profile:
#' sd_vvp_threshold(example_vp)
#'
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
  assert_that(is.numeric(value))
  assert_that(value > 0)
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
    stop("Input must be list of vp objects.")
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
  assert_that(is.numeric(value))
  assert_that(value > 0)
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
