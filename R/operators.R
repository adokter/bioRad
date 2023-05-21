#' Mathematical and arithmetic operations on param's, scan's and pvol's
#'
#' @param x object of class `scan`, or `pvol`
#' @param ... objects passed on to the Math functions
#' @param e1 object of class `param`, `scan`, `pvol` or a number
#' @param e2 object of class `param`, `scan`, `pvol` or a number
#'
#' @return an object of the input class
#'
#' @rdname Ops
#' @details
#' Use caution when applying these manipulations, as there are no
#' consistency checks if the operations lead to interpretable outcomes.
#' For example, when averaging scans with logarithmic values (e.g. DBZ), it might be required
#' to first exponentiate the data before summing.
#'
#' Attributes are taken from the first object in the operation.
#'
#' When a `pvol` is multiplied by a list, in which case arguments are taken from the list per scan.
#' this requires the list to have the same length as the number of scans.
#'
#' @export
#'
#' @seealso
#' * [calculate_param()]
#'
#' @examples
#' # Locate and read the polar volume example file
#' scan1 <- example_scan
#'
#' #add a value of 1 to all scan parameters:
#' scan2 <- example_scan + 1
#'
#' # average the scan parameters of two scans:
#' # NB: requires identical scan parameter names and order!
#' (scan1 + scan2)/2
`Math.scan` <- function(x, ...) {
  if (.Generic %in% c(
    "lgamma", "gamma", "digamma", "trigamma",
    "cumsum", "cumprod", "cummax", "cummin"
  )) {
    stop(paste("Operation", .Generic, "not defined for scan objects"))
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
    stop(paste("Operation", .Generic, "not defined for pvol objects"))
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
    if (!identical(dim(e1), dim(e2))) {
      stop("Scan parameters have different dimensions")
    }
    if (!isTRUE(all.equal(g1, g2))) {
      warning("The `geo` attribute of the parameter(s) differs. You are likely combining scan parameters with different elevations, radar locations or range/azimuth resolution")
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
        ") not defined for scans with unequal number of scan parameters"
      ))
    }
    if (any(names(e1$params) != names(e2$params)) & pmin(l1, l2) != 1) {
      warning(paste0(
        "Mathematical operation (", .Generic,") applied to scans with different scan parameter names, potentially combining distinct parameters."
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
        ") not defined for pvols with unequal number of scans"
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
          stop("pvol and list() multiplication requires a list length equal to the number of scans.")
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
        stop("pvol and list() multiplication requires a list length equal to the number of scans.")
      }
      e1$scans <- mapply(.Generic, e1$scans, e2, SIMPLIFY = F)
    } else {
      e1$scans <- lapply(e1$scans, .Generic, e2)
    }
    return(e1)
  }
}
