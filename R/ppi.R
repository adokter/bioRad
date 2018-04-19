#' Subset `ppi`
#'
#' Extract by index from a ppi
#'
#' @param x An object of class \code{param} or \code{scan}.
#' @param i Indices specifying elements to extract.
#'
#' @export
`[.ppi` <- function(x, i) {
  stopifnot(inherits(x, "ppi"))
  myppi <- list(radar = x$radar, datetime = x$datetime,
                data = x$data[i], geo = x$geo)
  class(myppi) <- "ppi"
  return(myppi)
}

#' Print method for ppi
#'
#' @param x An object of class \code{ppi}.
#'
#' @keywords internal
#'
#' @export
print.ppi <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "ppi"))
  cat("               Plan position indicator (class ppi)\n\n")
  cat("  quantities: ", names(x$data), "\n")
  cat("        dims: ", x$data@grid@cells.dim[1], "x",
      x$data@grid@cells.dim[2], "pixels\n\n")
}

#' Class 'ppi': plan position indicator
#'
#' @param object Object of class \code{ppi}.
#' @param x Object of class \code{ppi}.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary ppi
#'
#' @export
#'
#' @details
#' A PPI of class 'ppi' is a list containing:
#' \describe{
#'  \item{\code{data}}{an object of class \link[sp]{SpatialGridDataFrame}
#'    containing the georeferenced data. Commonly available parameters are:
#'     \describe{
#'      \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'      \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity [m/s]. Radial
#'        velocities towards the radar are negative, while radial velocities
#'        away from the radar are positive}
#'      \item{"\code{RHOHV}"}{Correlation coefficient [unitless]. Correlation
#'        between vertically polarized and horizontally polarized reflectivity
#'        factor}
#'      \item{"\code{PHIDP}"}{Differential phase [degrees]}
#'      \item{"\code{ZDR}"}{(Logged) differential reflectivity [dB]}
#'        }
#'  }
#'  \item{\code{geo}}{geographic data, a list with:
#'     \describe{
#'      \item{\code{lat}}{latitude of the radar [decimal degrees]}
#'      \item{\code{lon}}{longitude of the radar [decimal degrees]}
#'      \item{\code{height}}{height of the radar
#'        antenna [metres above sea level]}
#'      \item{\code{elangle}}{radar beam elevation [degrees]}
#'      \item{\code{rscale}}{range bin size [m]}
#'      \item{\code{ascale}}{azimuth bin size [deg]}
#'     }
#'     The \code{geo} element of a 'scan' object is a copy of the \code{geo}
#'     element of its parent scan or scan parameter.
#'   }
#' }
summary.ppi <- function(object, ...) {
  print.ppi(object)
}

#' @rdname summary.ppi
#'
#' @export
#'
#' @return For \code{is.ppi}: \code{TRUE} if its argument is of
#' class \code{ppi}.
is.ppi <- function(x) inherits(x, "ppi")

#' @rdname summary.ppi
#'
#' @export
#'
#' @return For \code{dim.ppi}: dimensions of the ppi.
dim.ppi <- function(x) {
  stopifnot(inherits(x, "ppi"))
  c(dim(x$data)[2], x$data@grid@cells.dim)
}

