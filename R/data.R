#' Example object of class \code{vp}
#'
#' Example of a \code{\link[=summary.vp]{vp}} object with name
#' \code{example_vp}. Can be created with \code{\link{calculate_vp}} or read
#' from file with \code{\link{read_vpfiles}}.
#'
#' @rdname example_vp
#'
#' @examples
#' # get summary of example vp:
#' summary(example_vp)
#'
#' # example_vp was created with:
#' vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
#' example_vp <- read_vpfiles(vpfile)
#' \dontrun{
#' save(example_vp, file = "data/example_vp.rda")
#' }
"example_vp"

#' Example object of class \code{scan}
#'
#' Example of a \code{\link[=summary.scan]{scan}} object with name
#' \code{example_scan}.
#'
#' @rdname example_scan
#'
#' @examples
#' # get summary of example scan:
#' summary(example_scan)
#'
#' # example_scan was created with:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' pvol <- read_pvolfile(pvolfile)
#' example_scan <- pvol$scans[[1]]
#' \dontrun{
#' save(example_scan, file = "data/example_scan.rda")
#' }
"example_scan"

#' Example object of class \code{vpts}
#'
#' Example of a \code{\link[=summary.vpts]{vpts}} object (a time series of
#' vertical profiles) with name \code{example_vpts}.
#'
#' @rdname example_vpts
#'
#' @examples
#' # get summary of example vpts:
#' summary(example_vpts)
#'
#' # example_vpts was created with:
#' vptsfile <- system.file("extdata", "vpts.txt", package = "bioRad")
#' example_vpts <- read_vpts(vptsfile, radar = "KBGM", wavelength = "S")
#' rcs(example_vpts) <- 11
#' example_vpts$attributes$where$lat <- 42.2
#' example_vpts$attributes$where$lon <- -75.98
#' \dontrun{
#' save(example_vpts, file = "data/example_vpts.rda", compress = "xz")
#' }
"example_vpts"
