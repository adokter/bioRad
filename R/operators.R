#' Mathematical and arithmetic operations on param's, scan's and pvol's
#'
#' @param x object of class \code{scan}, or \code{pvol}
#' @param ... parameters passed on to the Math functions
#' @param e1 object of class \code{param}, \code{scan}, \code{pvol} or a number
#' @param e2 object of class \code{param}, \code{scan}, \code{pvol} or a number
#'
#' @return an object of the input class
#'
#' @rdname Ops
#' @details
#' These function do not attempt to check if the operations are sensible.
#' For example when combining logarithmic values (e.g. DBZ) it might make
#' sense to first take the exponent them before summing them.
#'
#' @export
#'
`Math.scan` <- function(x, ...) {
  if (.Generic %in% c(
    "lgamma", "gamma", "digamma", "trigamma",
    "cumsum", "cumprod", "cummax", "cummin"
  )) {
    warning(paste("Operation", .Generic, "not meaningful for scan objects"))
  }
  x$params <- lapply(x$params, .Generic)
  x
}

#' @export
#' @rdname Ops
`Math.pvol` <- function(x, ...) {
  if (.Generic %in% c(
    "lgamma", "gamma", "digamma", "trigamma",
    "cumsum", "cumprod", "cummax", "cummin"
  )) {
    warning(paste("Operation", .Generic, "not meaningful for pvol objects"))
  }
  x$scans <- lapply(x$scans, .Generic)
  x
}

#' @rdname Ops
#' @export
`Ops.param` <- function(e1, e2) {
  m <- do.call(.Generic, list(c(e1), c(e2)))
  if (is.param(e1)) {
    e1[] <- m
    return(e1)
  } else {
    e2[] <- m
    return(e2)
  }
}

#' @export
#' @rdname Ops

`Ops.scan` <- function(e1, e2) {
  if (is.scan(e1)) {
    if (is.scan(e2)) {
      if(any(names(e1$params)!=names(e2$params)))
      {
        warning(paste0("The names of parameters do not match, this
                       likely means you try to combine (",.Generic,
                       ") different quantities."))
      }
      e1$params <- mapply(.Generic, e1$params, e2$params, SIMPLIFY = F)
      return(e1)
    } else {
      e1$params <- mapply(.Generic, e1$params, e2, SIMPLIFY = F)
      return(e1)
    }
  } else {
    n <- names(e2$params)
    e2$params <- mapply(.Generic, e1, e2$params, SIMPLIFY = F)
    names(e2$params) <- n
    return(e2)
  }
}


#' @rdname Ops
#' @export
`Ops.pvol` <- function(e1, e2) {
  if (is.pvol(e1)) {
    if (is.pvol(e2)) {
      e1$scans <- mapply(.Generic, e1$scans, e2$scans, SIMPLIFY = F)
      return(e1)
    } else {
      e1$scans <- mapply(.Generic, e1$scans, e2, SIMPLIFY = F)
      return(e1)
    }
  } else {
    n <- names(e2$scans)
    e2$scans <- mapply(.Generic, e1, e2$scans, SIMPLIFY = F)
    names(e2$scans) <- n
    return(e2)
  }
}


