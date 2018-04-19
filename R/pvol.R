#' Object of class \code{pvol}: a polar volume
#'
#' @param object Object of class \code{pvol}.
#' @param x Object of class \code{pvol}.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary pvol
#'
#' @export
#'
#' @details
#' A polar scan object of class \code{pvol} is a list containing:
#' \describe{
#'  \item{\code{radar}}{character string with the radar identifier}
#'  \item{\code{datetime}}{nominal time of the volume [UTC]}
#'  \item{\code{scans}}{a list with scan objects of class 'scan'}
#'  \item{\code{attributes}}{list with the volume's \code{\\what},
#'    \code{\\where} and \code{\\how} attributes}
#'  \item{\code{geo}}{geographic data, a list with:
#'   \describe{
#'      \item{\code{lat}}{latitude of the radar [decimal degrees]}
#'      \item{\code{lon}}{longitude of the radar [decimal degrees]}
#'      \item{\code{height}}{height of the radar
#'        antenna [metres above sea level]}
#'   }
#'  }
#' }
#'
#' @examples
#' # locate example volume file:
#' pvol <- system.file("extdata", "volume.h5", package = "bioRad")
#' # print the local path of the volume file:
#' pvol
#' # load the file:
#' vol <- read.pvol(pvol)
#' # print summary info for the loaded polar volume:
#' vol
#' # print summary info for the scans in the polar volume:
#' vol$scans
#' # copy the first scan to a new object 'scan'
#' scan <- vol$scans[[1]]
summary.pvol <- function(object, ...) {
  print.pvol(object)
}

#' @rdname summary.pvol
#'
#' @return for \code{is.pvol}: \code{TRUE} if its argument is of
#' class \code{pvol}
#'
#' @export
#'
#' @examples
#' is.pvol("this is not a polar volume but a string")  #> FALSE
is.pvol <- function(x) inherits(x, "pvol")


#' Print method for class \code{pvol}
#'
#' @param x An object of class \code{pvol}, a polar volume
#'
#' @keywords internal
#'
#' @export
print.pvol <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "pvol"))
  cat("               Polar volume (class pvol)\n\n")
  cat("     # scans: ", length(x$scans), "\n")
  cat("       radar: ", x$radar, "\n")
  cat("      source: ", x$attributes$what$source, "\n")
  cat("nominal time: ", as.character(x$datetime), "\n\n")
}
