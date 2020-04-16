#' Inspect a parameter (`param`)
#'
#' R base functions for inspecting a parameter (`param`) object.
#'
#' @param x A `param` object.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary param
#'
#' @export
#'
#' @details
#' A parameter is a quantity/variable measured by the radar during a scan (or
#' sweep). These are organized along radar range (bins) and azimuth (rays). Scan
#' parameters are named according to the OPERA data information model (ODIM),
#' see Table 16 in the [ODIM
#' specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf).
#'
#' Commonly available parameters are:
#' * `DBZH`, `DBZ`: (Logged) reflectivity factor in dBZ.
#' * `VRADH`, `VRAD`: Radial velocity in m/s. Radial velocities towards the
#' radar are negative, while radial velocities away from the radar are positive.
#' * `RHOHV`: Correlation coefficient (unitless). Correlation between the
#' vertically and horizontally polarized reflectivity factor.
#' * `PHIDP`: Differential phase in degrees.
#' * `ZDR`: (Logged) differential reflectivity in dB.
#'
#' @seealso [get_param()]
#'
#' @examples
#' # Load example scan
#' data(example_scan)
#'
#' # Extract the DBZH scan parameter
#' example_param <- get_param(example_scan, "DBZH")
#'
#' # Verify that is an object of class param
#' is.param(example_param)
#'
#' # Get summary info for this parameter
#' example_param # Same as summary(example_param) or print(example_param)
summary.param <- function(x, ...) {
  print.param(x)
}

#' Verify if an object is of class `param`
#'
#' @inheritParams summary.param
#'
#' @return For [is.param()]: `TRUE` for an object of class `param`, otherwise
#'   `FALSE`.
#'
#' @rdname summary.param
#'
#' @export
is.param <- function(x) {
  inherits(x, "param")
}

#' Print summary for an object of class `param`
#'
#' @inheritParams summary.param
#'
#' @rdname summary.param
#'
#' @export
print.param <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "param"))
  cat("               Polar scan parameter (class param)\n\n")
  cat("    quantity: ", attributes(x)$param, "\n")
  cat("        dims: ", dim(x)[1], "bins x", dim(x)[2], "rays\n")
}
