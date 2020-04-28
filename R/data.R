#' Scan (`scan`) example
#'
#' Example object of class [`scan`][summary.scan()] with name `example_scan`.
#'
#' @rdname example_scan
#'
#' @seealso [summary.scan]
#'
#' @examples
#' # Reload example_scan from package (e.g. in case it was altered)
#' data(example_scan)
#'
#' # Get summary info
#' example_scan
#'
#' # example_scan was created with
#' \dontrun{
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' pvol <- read_pvolfile(pvolfile)
#' example_scan <- pvol$scans[[1]]
#' save(example_scan, file = "data/example_scan.rda")
#' }
"example_scan"

#' Vertical profile (`vp`) example
#'
#' Example object of class [`vp`][summary.vp()] with name `example_vp`.
#'
#' @rdname example_vp
#'
#' @examples
#' # Reload example_vp from package (e.g. in case it was altered)
#' data(example_vp)
#'
#' # Get summary info
#' example_vp
#'
#' # example_vp was created with
#' \dontrun{
#' vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
#' example_vp <- read_vpfiles(vpfile)
#' save(example_vp, file = "data/example_vp.rda")
#' }
"example_vp"

#' Time series of vertical profiles (`vpts`) example
#'
#' Example object of class [`vpts`](summary.vpts()] with name `example_vpts`.
#'
#' @rdname example_vpts
#'
#' @seealso [summary.vpts]
#'
#' @examples
#' # Reload example_vpts from package (e.g. in case it was altered)
#' data(example_vpts)
#'
#' # Get summary info
#' example_vpts
#'
#' # example_vpts was created with
#' \dontrun{
#' vptsfile <- system.file("extdata", "vpts.txt.zip", package = "bioRad")
#' unzip(vptsfile, exdir = (dirname(vptsfile)), junkpaths = TRUE)
#' vptsfile <- substr(vptsfile, 1, nchar(vptsfile) - 4)
#' example_vpts <- read_vpts(vptsfile, radar = "KBGM", wavelength = "S")
#' rcs(example_vpts) <- 11
#' sd_vvp_threshold(example_vpts) <- 2
#' example_vpts$attributes$where$lat <- 42.2
#' example_vpts$attributes$where$lon <- -75.98
#' save(example_vpts, file = "data/example_vpts.rda", compress = "xz")
#' }
"example_vpts"
