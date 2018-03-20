#' Class 'param': polar scan parameter
#' @param object object of class 'param'
#' @param x object of class 'param'
#' @param ... additional arguments affecting the summary produced.
#' @export
#' @method summary param
#' @details
#' Scan parameters are simple matrices, with the following specific attributes:
#' \describe{
#'    \item{\code{lat}}{latitude of the radar [decimal degrees]}
#'    \item{\code{lon}}{longitude of the radar [decimal degrees]}
#'    \item{\code{height}}{height of the radar antenna [metres above sea level]}
#'    \item{\code{elangle}}{radar beam elevation [degrees]}
#'    \item{\code{param}}{string with the name of the polar scan parameter}
#' }
#' Scan parameters are named according to the OPERA data information model (ODIM), see
#' Table 16 in the \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
#' Commonly available parameters are:
#' \describe{
#'  \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'  \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity [m/s]. Radial velocities towards
#'   the radar are negative, while radial velocities away from the radar are positive}
#'  \item{"\code{RHOHV}"}{Correlation coefficient [unitless]. Correlation between vertically polarized and horizontally polarized reflectivity factor}
#'  \item{"\code{PHIDP}"}{Differential phase [degrees]}
#'  \item{"\code{ZDR}"}{(Logged) differential reflectivity [dB]}
#' }
summary.param=function(object, ...) print.param(object)

#' @rdname summary.param
#' @export
#' @return for \code{is.scan}: \code{TRUE} if its argument is of class "\code{param}"
#' @examples
#' is.param("this is not a polar scan parameter but a string")  #> FALSE
is.param <- function(x) inherits(x, "param")

#' print method for class \code{param}
#'
#' @param x An object of class \code{param}, a polar scan parameter
#' @keywords internal
#' @export
print.param=function(x,digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "param"))
  cat("               Polar scan parameter (class param)\n\n")
  cat("    quantity: ",attributes(x)$param,"\n")
  cat("        dims: ",dim(x)[1],"bins x",dim(x)[2],"rays\n")
}
