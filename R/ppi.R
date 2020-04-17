#' Inspect a plan position indicator (`ppi`)
#'
#' R base functions for inspecting a plan position indicator (`ppi`) object.
#'
#' @param x A `ppi` object.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary ppi
#'
#' @export
#'
#' @details
#' A plan position indicator is a projection of radar data onto the earth's
#' surface, generated from a single scan (`scan`) with [project_as_ppi()], a
#' polar volume (`pvol`) with [integrate_to_ppi()] or multiple plan position
#' indicators (`ppi`) with [composite_ppi()]. A plan position indicator (`ppi`)
#' object is a list containing:
#' * `radar`: Radar identifier.
#' * `datetime`: Nominal time of the volume to which the scan belongs in UTC.
#' * `data`: A [`sp::SpatialGridDataFrame`] containing the georeferenced data.
#' See [summary.param()] for commonly available parameters, such as `DBZH`.
#' * `geo`: List of the scan's geographic properties (see the `geo` element in
#' [summary.scan()]), with two additional properties:
#'   * `bbox`: Bounding box for the plan position indicator in decimal degrees.
#'   * `merged`: Logical. Flag to indicate if a plan position indicator is a
#'   composite of multiple scans. `TRUE` if generated with [integrate_to_ppi()]
#'   or [composite_ppi()].
#'
#' @seealso
#' * [project_as_ppi()]
#' * [integrate_to_ppi()]
#' * [plot.ppi()]
#' * [map()]
#' * [composite_ppi()]
#' * \code{\link[=[.ppi]{[ppi()}}
#'
#' @examples
#' # Load the example scan
#' data(example_scan)
#'
#' # Project scan as ppi
#' example_ppi <- project_as_ppi(example_scan)
#'
#' # Verify that it is an object of class ppi
#' is.ppi(example_ppi)
#'
#' # Get summary info
#' example_ppi # Same as summary(example_ppi) or print(example_ppi)
#'
#' # Get dimensions
#' dim(example_ppi)
summary.ppi <- function(x, ...) {
  print.ppi(x)
}

#' Print summary for an object of class `ppi`
#'
#' @inheritParams summary.ppi
#'
#' @rdname summary.ppi
#'
#' @export
print.ppi <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "ppi"))
  cat("               Plan position indicator (class ppi)\n\n")
  cat("  parameters: ", names(x$data), "\n")
  cat(
    "        dims: ", x$data@grid@cells.dim[1], "x",
    x$data@grid@cells.dim[2], "pixels\n\n"
  )
}

#' Verify if an object is of class `ppi`
#'
#' @inheritParams summary.ppi
#'
#' @return For [is.ppi()]: `TRUE` for an object of class `ppi`, otherwise
#'   `FALSE`.
#'
#' @rdname summary.ppi
#'
#' @export
is.ppi <- function(x) {
  inherits(x, "ppi")
}

#' Get dimensions for an object of class `ppi`
#'
#' @return For [dim.ppi()]: number of parameters (`param`), x and y pixels in a
#'   plan position indicator (`ppi`).
#'
#' @rdname summary.ppi
#'
#' @export
dim.ppi <- function(x) {
  stopifnot(inherits(x, "ppi"))
  c(dim(x$data)[2], x$data@grid@cells.dim)
}

#' Subset a plan position indicator (`ppi`)
#'
#' Select parameters (`param`) or derived quantities by index from a plan
#' position indicator (`ppi`).
#'
#' @param x A `ppi` object.
#' @param i Integer. Index/indices specifying which parameters (`param`) or
#'   derived quantities to extract.
#'
#' @export
#'
#' @examples
#' # Load the example scan
#' data(example_scan)
#'
#' # Project scan as ppi
#' example_ppi <- project_as_ppi(example_scan)
#'
#' # This ppi contains 5 parameters (VRADH DBZH ZDR RHOHV PHIDP)
#' example_ppi
#'
#' # Subset ppi to one containing only the first parameter (VRADH)
#' example_ppi[1]
#'
#' # Subset ppi to one containing the first three parameters (VRADH, DBZH, ZDR)
#' example_ppi[1:3]
`[.ppi` <- function(x, i) {
  stopifnot(inherits(x, "ppi"))
  my_ppi <- list(
    radar = x$radar, datetime = x$datetime,
    data = x$data[i], geo = x$geo
  )
  class(my_ppi) <- "ppi"
  return(my_ppi)
}
