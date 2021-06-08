#' Inspect a parameter (`param`)
#'
#' R base functions for inspecting a parameter (`param`) object.
#'
#' @param object A `param` object.
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
#' * `TH`, `T`: (Logged) uncorrected reflectivity factor in dBZ.
#' * `VRADH`, `VRAD`: Radial velocity in m/s. Radial velocities towards the
#' radar are negative, while radial velocities away from the radar are positive.
#' * `RHOHV`: Correlation coefficient (unitless). Correlation between the
#' vertically and horizontally polarized reflectivity factor.
#' * `PHIDP`: Differential phase in degrees.
#' * `ZDR`: (Logged) differential reflectivity in dB.
#'
#' @seealso
#' * [get_param()]
#'
#' @examples
#' # Extract the DBZH scan parameter from the example scan
#' param <- get_param(example_scan, "DBZH")
#'
#' # Verify that it is an object of class param
#' is.param(param)
#'
#' # Get summary info for this parameter
#' param # Same as summary(param) or print(param)
summary.param <- function(object, ...) {
  print.param(object)
}

#' Print summary for an object of class `param`
#'
#' @noRd
#'
#' @export
print.param <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "param"))
  cat("               Polar scan parameter (class param)\n\n")
  cat("    quantity: ", attributes(x)$param, "\n")
  cat("        dims: ", dim(x)[1], "bins x", dim(x)[2], "rays\n")
}

#' Verify if an object is of class `param`
#'
#' @param x A `param` object.
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
