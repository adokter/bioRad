#' Class \code{ppi}: a plan position indicator
#'
#' Class \code{ppi} for a plan position indicator, and its associated R base
#' functions.
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
#' \code{ppi} objects are generated from elevation scans (\code{scan} objects)
#' with \link{project_as_ppi} or from polar volumes with \link{integrate_to_ppi},
#' producing projections of the radar data onto the earth's surface.
#'
#' An object of class \code{ppi} is a list containing:
#' \describe{
#'  \item{\code{data}}{an object of class \link[sp]{SpatialGridDataFrame}
#'    containing the georeferenced data. Commonly available parameters are:
#'     \describe{
#'      \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'      \item{"\code{TH}", "\code{T}"}{(Logged) uncorrected reflectivity factor [dBZ]}
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
#'        antenna [meters above sea level]}
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

#' Subset a plan position indicator (\code{ppi})
#'
#' Select quantities by index from a \code{ppi}
#'
#' @param x An object of class \code{param} or \code{scan}.
#' @param i Indices specifying elements to extract.
#'
#' @export
#' @examples
#' # make a ppi:
#' my_ppi <- project_as_ppi(example_scan)
#'
#' # this ppi contains 5 quantities (VRADH DBZH ZDR RHOHV PHIDP):
#' my_ppi
#'
#' # This ppi only contains the first quantity (VRADH):
#' my_ppi[1]
#'
#' # This ppi contains the first three quantities (VRADH, DBZH, ZDR):
#' my_ppi[1:3]
`[.ppi` <- function(x, i) {
  stopifnot(inherits(x, "ppi"))
  myppi <- list(
    radar = x$radar, datetime = x$datetime,
    data = x$data[i], geo = x$geo
  )
  class(myppi) <- "ppi"
  return(myppi)
}

#' Print method for class \code{ppi}
#'
#' @param x An object of class \code{ppi}.
#'
#' @keywords internal
#'
#' @export
print.ppi <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "ppi"))
  cat("               Plan position indicator (class ppi)\n\n")
  cat("  quantities: ", names(x$data), "\n")
  cat(
    "        dims: ", x$data@grid@cells.dim[1], "x",
    x$data@grid@cells.dim[2], "pixels\n\n"
  )
}
