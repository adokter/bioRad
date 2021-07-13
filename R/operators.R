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
#' When doing operations on object of length one the arguments are recycled.
#'
#' Attributes are taken from the first object in the operation.
#'
#' If a \code{pvol} is multiplied by a list arguments are taken from the list per scan.
#' this requires the list to have the same length as the number of scans.
#'
#' @export
#'
`Math.scan` <- function(x, ...) {
  if (.Generic %in% c(
    "lgamma", "gamma", "digamma", "trigamma",
    "cumsum", "cumprod", "cummax", "cummin"
  )) {
    stop(paste("Operation", .Generic, "not meaningful for scan objects"))
  }
  x$params <- lapply(x$params, .Generic, ...)
  x
}

#' @export
#' @rdname Ops
`Math.pvol` <- function(x, ...) {
  if (.Generic %in% c(
    "lgamma", "gamma", "digamma", "trigamma",
    "cumsum", "cumprod", "cummax", "cummin"
  )) {
    stop(paste("Operation", .Generic, "not meaningful for pvol objects"))
  }
  x$scans <- lapply(x$scans, .Generic, ...)
  x
}

#' @rdname Ops
#' @export
`Ops.param` <- function(e1, e2) {
  if (is.param(e1) & is.param(e2)) {
    g1 <- attr(e1, "geo")
    g2 <- attr(e2, "geo")
    if (!isTRUE(all.equal(g1, g2))) {
      warning("The `geo` attributed of the parameter(s) differ (e.g., location, range gate location) meaning you are likly to combine data that has been observed in different places.")
    }
    if (!identical(dim(e1), dim(e2))) {
      stop("The parameters have different dimensions, means the result will not be a valid parameter.")
    }
  }
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
  if (is.scan(e1) & is.scan(e2)) {
    l1 <- length(e1$params)
    l2 <- length(e2$params)
    if (l1 != l2 & pmin(l1, l2) != 1) {
      stop(paste0(
        "Operation (", .Generic,
        ") does not work for scans with unequal number of parameters (except for length one)"
      ))
    }
    if (any(names(e1$params) != names(e2$params)) & pmin(l1, l2) != 1) {
      warning(paste0(
        "The names of parameters do not match, this
                       likely means you try to combine (", .Generic,
        ") different quantities."
      ))
    }
    warn <- NULL # mechanism to prevent duplicated warnings
    e1$params <- withCallingHandlers(mapply(.Generic, e1$params, e2$params, SIMPLIFY = F), warning = function(e) {
      warn <<- append(warn, list(e))
      invokeRestart("muffleWarning")
    })
    lapply(unique(warn), warning)
    return(e1)
  } else {
    if (is.scan(e2)) {
      e2$params <- lapply(e2$params,
        function(gen, e1, e2) {
          do.call(gen, list(e1, e2))
        },
        gen = .Generic, e1 = e1
      )
      return(e2)
    }
    e1$params <- lapply(e1$params, .Generic, e2)
    return(e1)
  }
}


#' @rdname Ops
#' @export
`Ops.pvol` <- function(e1, e2) {
  if (is.pvol(e1) & is.pvol(e2)) {
    if (length(e1$scans) != length(e2$scans)) {
      stop(paste0(
        "Operation (", .Generic,
        ") does not work for pvols with unequal number of scans"
      ))
    }
    warn <- NULL # mechanism to prevent duplicated warnings
    e1$scans <- withCallingHandlers(mapply(.Generic, e1$scans, e2$scans, SIMPLIFY = F), warning = function(e) {
      warn <<- append(warn, list(e))
      invokeRestart("muffleWarning")
    })
    lapply(unique(warn), warning)
    return(e1)
  } else { # there is one non scan
    if (is.pvol(e2)) {
      if (is.list(e1)) {
        if (length(e2$scans) != length(e1)) {
          stop("Multiplying by lists only works if the list is equally long to the number of scans.")
        }
        e2$scans <- mapply(.Generic, e2$scans, e1, SIMPLIFY = F)
      } else {
        e2$scans <- lapply(e2$scans, function(gen, e1, e2) {
          do.call(gen, list(e1, e2))
        }, gen = .Generic, e1 = e1)
      }
      return(e2)
    }
    if (is.list(e2)) {
      if (length(e1$scans) != length(e2)) {
        stop("Multiplying by lists only works if the list is equally long to the number of scans.")
      }
      e1$scans <- mapply(.Generic, e1$scans, e2, SIMPLIFY = F)
    } else {
      e1$scans <- lapply(e1$scans, .Generic, e2)
    }
    return(e1)
  }
}
