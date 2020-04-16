#' Get radar cross section
#'
#' Gives the currently assumed radar cross section of an object in cm^2.
#'
#' @param x A `vp`, list of `vp`, `vpts` or `vpi` object.
#'
#' @return The radar cross section in cm^2.
#'
#' @export
#'
#' @seealso [`rcs()<-`] for setting the radar cross section of an object.
#'
#' @examples
#' # Get the radar cross section for a vp
#' rcs(example_vp)
#'
#' # Get the radar cross section for a vpts
#' rcs(example_vpts)
#'
#' # Get the radar cross section for a vpi
#' example_vpi <- integrate_profile(example_vpts)
#' rcs(example_vpi)
rcs <- function(x) {
  UseMethod("rcs", x)
}

#' @rdname rcs
#'
#' @export
rcs.vp <- function(x) {
  stopifnot(inherits(x, "vp"))
  x$attributes$how$rcs_bird
}

#' @rdname rcs
#'
#' @export
rcs.list <- function(x) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("Input must be list of vp objects.")
  }
  output <- sapply(x, `rcs.vp`)
  output
}

#' @rdname rcs
#'
#' @export
rcs.vpts <- function(x) {
  stopifnot(inherits(x, "vpts"))
  x$attributes$how$rcs_bird
}

#' @rdname rcs
#'
#' @export
rcs.vpi <- function(x) {
  stopifnot(inherits(x, "vpi"))
  attributes(x)$rcs
}

#' Set radar cross section
#'
#' Sets the assumed radar cross section of an object in cm^2. This method also
#' updates the migration densities in `x$data$dens`.
#'
#' @inheritParams rcs
#' @param value Double. The radar cross section value to assign in cm^2.
#'
#' @export
#'
#' @seealso [rcs()] for getting the radar cross section of an object.
#'
#' @examples
#' # Set the radar cross section for a vp
#' rcs(example_vp) <- 11
#'
#' # Set the radar cross section for a vpts
#' rcs(example_vpts) <- 11
#'
#' # Set the radar cross section for a vpi
#' example_vpi <- integrate_profile(example_vpts)
#' rcs(example_vpi) <- 11
`rcs<-` <- function(x, value) {
  UseMethod("rcs<-", x)
}

#' @rdname rcs-set
#'
#' @export
`rcs<-.vp` <- function(x, value) {
  stopifnot(inherits(x, "vp"))
  assert_that(is.numeric(value))
  assert_that(value > 0)
  x$attributes$how$rcs_bird <- value
  x$data$dens <- x$data$eta / value
  if (is.numeric(x$attributes$how$sd_vvp_thresh)) {
    x$data$dens[x$data$sd_vvp < x$attributes$how$sd_vvp_thresh] <- 0
  } else {
    warning("Threshold for sd_vvp not set, defaulting to 2 m/s.")
    x$attributes$how$sd_vvp_thresh <- 2
    x$data$dens[x$data$sd_vvp < 2] <- 0
  }
  x
}

#' @rdname rcs-set
#'
#' @export
`rcs<-.list` <- function(x, value) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("Input must be list of vp objects.")
  }
  output <- lapply(x, `rcs<-.vp`, value = value)
  class(output) <- c("list")
  output
}

#' @rdname rcs-set
#'
#' @export
`rcs<-.vpts` <- function(x, value) {
  stopifnot(inherits(x, "vpts"))
  assert_that(is.numeric(value))
  assert_that(value > 0)
  x$attributes$how$rcs_bird <- value
  x$data$dens <- x$data$eta / value
  if (is.numeric(x$attributes$how$sd_vvp_thresh)) {
    x$data$dens[x$data$sd_vvp < x$attributes$how$sd_vvp_thresh] <- 0
  } else {
    warning("Threshold for sd_vvp not set, defaulting to 2 m/s.")
    x$attributes$how$sd_vvp_thresh <- 2
    x$data$dens[x$data$sd_vvp < 2] <- 0
  }
  x
}

#' @rdname rcs-set
#'
#' @export
`rcs<-.vpi` <- function(x, value) {
  stopifnot(inherits(x, "vpi"))
  attributes(x)$rcs <- value
  x$mtr <- x$rtr / value
  x$vid <- x$vir / value
  x
}
