#' Get threshold of the radial velocity standard deviation
#'
#' Gives the current threshold of the radial velocity standard deviation
#' (`sd_vvp`) of an object in m/s, retrieved by velocity volume processing (VVP).
#'
#' @param x A `vp`, list of `vp` or `vpts` object.
#'
#' @return The `sd_vvp` threshold in m/s.
#'
#' @export
#'
#' @seealso
#' * [`sd_vvp_threshold()<-`][sd_vvp_threshold<-] for setting the `sd_vvp`
#' threshold of an object.
#' * [rcs()]
#'
#' @examples
#' # Get the sd_vvp threshold for a vp
#' vp <- example_vp
#' sd_vvp_threshold(vp)
#'
#' # Get the sd_vvp threshold for a vpts
#' vpts <- example_vpts
#' sd_vvp_threshold(vpts)
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

#' Set threshold of the radial velocity standard deviation
#'
#' Sets the threshold of radial velocity standard deviation (`sd_vvp`) of an
#' object in m/s. Altitude layers with `sd_vvp` below this threshold are assumed
#' to have an aerial density of zero individuals. This function also updates the
#' migration densities in `x$data$dens` to `eta`/`rcs` when above
#' `sd_vvp_threshold` and `0` if below.
#'
#' @inheritParams sd_vvp_threshold
#' @param value Double. The `sd_vvp` threshold value to assign in m/s.
#'
#' @export
#'
#' @seealso
#' * [sd_vvp_threshold()] for getting the `sd_vvp` threshold of an object.
#' * [`rcs()<-`][rcs<-]
#'
#' @examples
#' # Set the sd_vvp threshold for a vp
#' vp <- example_vp
#' sd_vvp_threshold(vp) <- 2
#'
#' # Set the sd_vvp threshold for a vpts
#' vpts <- example_vpts
#' sd_vvp_threshold(vpts) <- 2
`sd_vvp_threshold<-` <- function(x, value) {
  UseMethod("sd_vvp_threshold<-", x)
}

#' @rdname sd_vvp_threshold-set
#'
#' @export
`sd_vvp_threshold<-.vp` <- function(x, value) {
  stopifnot(inherits(x, "vp"))
  assert_that(is.numeric(value))
  assert_that(value >= 0)
  x$attributes$how$sd_vvp_thresh <- value
  if (is.numeric(x$attributes$how$rcs_bird)) {
    x$data$dens <- x$data$eta / x$attributes$how$rcs_bird
    x$data$dens[x$data$sd_vvp < value] <- 0
  } else {
    warning("Radar cross section not set, defaulting to 11 cm^2.")
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
  assert_that(value >= 0)
  x$attributes$how$sd_vvp_thresh <- value
  if (is.numeric(x$attributes$how$rcs_bird)) {
    x$data$dens <- x$data$eta / x$attributes$how$rcs_bird
    x$data$dens[x$data$sd_vvp < value] <- 0
  } else {
    warning("Radar cross section not set, defaulting to 11 cm^2.")
    x$data$dens <- x$data$eta / 11
    x$attributes$how$rcs_bird <- 11
    x$data$dens[x$data$sd_vvp < value] <- 0
  }
  x
}
