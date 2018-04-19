#' Example object of class \code{vp}
#'
#' Example of a \code{\link[=summary.vp]{vp}} object with name \code{VP}. Can
#' be created with \code{\link{calculate_vp()}} or read from file with
#' \code{\link{read_vpfiles()}}.
#'
#' @rdname vp_dataset
#'
#' @examples
#' # get summary of example vp:
#' summary(VP)
#'
#' # VP was created with:
#' VP <- read_vpfiles("/inst/extdata/profile.h5")
#' save(VP, file = "data/VP.RData")
"VP"

#' Example object of class \code{scan}
#'
#' Example of a \code{\link[=summary.scan]{scan}} object with name \code{SCAN}.
#'
#' @rdname scan_dataset
#'
#' @examples
#' # get summary of example scan:
#' summary(SCAN)
#'
#' # SCAN was created with:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' pvol <- read_pvolfile(pvolfile)
#' SCAN <- pvol$scans[[1]]
#' save(SCAN, file = "data/SCAN.RData")
"SCAN"

#' Example object of class \code{vpts}
#'
#' Example of a \code{\link[=summary.vpts]{vpts}} object (a time series of
#' vertical profiles) with name \code{SCAN}.
#'
#' @rdname vpts_dataset
#'
#' @examples
#' # get summary of example vpts:
#' summary(VPTS)
#'
#' # VPTS was created with:
#' vptsfile <- system.file("extdata", "VPtable.txt", package = "bioRad")
#' VPTS <- read_vpts(vptsfile, radar = "KBGM", wavelength = "S")
#' rcs(VPTS) <- 11
#' VPTS$attributes$where$lat <- 42.2
#' VPTS$attributes$where$lon <- -75.98
#' save(VPTS, file = "data/VPTS.RData", compress = "xz")
"VPTS"
