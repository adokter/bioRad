#' Scan (`scan`) example
#'
#' Example of a [`scan`][summary.scan()] object with name `example_scan`.
#'
#' @rdname example_scan
#' @return An example object of type `scan` which represents a single scan from a weather radar.
#' @seealso
#' * [summary.scan()]
#'
#' @examples
#' # Reload example_scan from package (e.g. in case it was altered)
#' data(example_scan)
#'
#' # Get summary info
#' example_scan
"example_scan"

#' Vertical profile (`vp`) example
#'
#' Example of a [`vp`][summary.vp()] object with name `example_vp`.
#'
#' @rdname example_vp   
#' @return An example object of type `vp` which represents a vertical profile.
#' @seealso
#' * [summary.vp()]
#'
#' @examples
#' # Reload example_vp from package (e.g. in case it was altered)
#' data(example_vp)
#'
#' # Get summary info
#' example_vp
"example_vp"

#' Time series of vertical profiles (`vpts`) example
#'
#' Example of a [`vpts`][summary.vpts()] object with name `example_vpts`.
#'
#' @rdname example_vpts
#' @return An example object of type `vpts` which represents a time series of vertical profiles.
#' @seealso
#' * [summary.vpts()]
#'
#' @examples
#' # Reload example_vpts from package (e.g. in case it was altered)
#' data(example_vpts)
#'
#' # Get summary info
#' example_vpts
"example_vpts"

## example_scan was created with:
# pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
# pvol <- read_pvolfile(pvolfile)
# example_scan <- pvol$scans[[1]]
# save(example_scan, file = "data/example_scan.rda")

## example_vp was created with:
# vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
# example_vp <- read_vpfiles(vpfile)
# save(example_vp, file = "data/example_vp.rda")

## example_vpts was created with
# vptsfile <- system.file("extdata", "vpts.txt.zip", package = "bioRad")
# utils::unzip(vptsfile, exdir = (dirname(vptsfile)), junkpaths = TRUE)
# vptsfile <- substr(vptsfile, 1, nchar(vptsfile) - 4)
# example_vpts <- read_vpts(vptsfile, radar = "KBGM", wavelength = "S")
# rcs(example_vpts) <- 11
# sd_vvp_threshold(example_vpts) <- 2
# example_vpts$attributes$where$lat <- 42.2
# example_vpts$attributes$where$lon <- -75.98
# save(example_vpts, file = "data/example_vpts.rda", compress = "xz")
