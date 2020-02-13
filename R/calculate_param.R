#' Calculate a new scan parameter
#'
#' Calculates a new scan parameter from a combination of existing scan parameters. Useful
#' for calculating quantities that are defined in terms of other basic radar moments, like
#' linear reflectivity eta, depolarization ratio (Kilambi et al. 2018), or for applying
#' clutter corrections (CCORH) to uncorrected reflectivity moments (TH), as in TH+CCORH
#' @param x an object of class \code{pvol} or class \code{scan}
#' @param ... an expression defining the new scan parameter in terms of existing scan parameters
#' @return an object of the same class as \code{x}, either class \code{pvol} or class \code{scan}
#' @export
#' @examples
#' # locate example volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # load the file:
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # calculate linear reflectivity ETA from reflectivity factor DBZH:
#' radar_wavelength <- example_pvol$attributes$how$wavelength
#' example_pvol <- calculate_param(example_pvol,ETA=dbz_to_eta(DBZH,radar_wavelength))
#'
#' # add depolarization ratio (DR) as a scan parameter (see Kilambi 2018):
#' example_pvol <- calculate_param(example_pvol, DR = 10 * log10((ZDR + 1 - 2 * ZDR^0.5 * RHOHV) /
#' (ZDR + 1 + 2 * ZDR^0.5 * RHOHV)))
#'
#' # calculate_param operates on both pvol and scan objects:
#' calculate_param(example_scan, DR = 10 * log10((ZDR + 1 - 2 * ZDR^0.5 * RHOHV) /
#' (ZDR + 1 + 2 * ZDR^0.5 * RHOHV)))
#' @references
#' \itemize{
#'   \item Kilambi, A., Fabry, F., and Meunier, V., 2018. A simple and effective method
#'   for separating meteorological from nonmeteorological targets using dual‐polarization
#'   data. Journal of Atmospheric and Oceanic Technology, 35, 1415–1424.
#'   \url{https://doi.org/10.1175/JTECH-D-17-0175.1}
#' }
calculate_param <- function(x, ...) {
  if (class(x) == "pvol") {
    x$scans <- do.call(lapply, list(x$scans, calculate_param, substitute(list(...))))
    return(x)
  }
  stopifnot(class(x) == "scan")
  if (as.character(as.list(substitute(...))[[1L]]) == "list") {
    calc <- as.list(substitute(...))[-1L]
  } else {
    calc <- as.list(substitute(list(...)))[-1L]
  }
  name <- names(calc)
  if (is.null(name)) {
    name <- rep("", length(calc))
  }
  for (i in seq_along(calc)) {
    newParam <- eval(nn <- (calc[[i]]), x$params)
    if ("" == (name[[i]])) {
      name[[i]] <- deparse(nn)
    }
    attr(newParam, "param") <- name[[i]]
    x$params[[name[[i]]]] <- newParam
  }
  return(x)
}
