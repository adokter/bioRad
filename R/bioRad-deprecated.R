## bioRad-deprecated.r
#' @title Deprecated functions in package \pkg{bioRad}.
#' @description The functions listed below are deprecated or renamed and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned.
#' @name bioRad-deprecated
#' @keywords internal
NULL

#' @name beamheight-deprecated
#' @rdname bioRad-deprecated
#' @section \code{beamheight}:
#' For \code{beamheight}, use \code{\link{beam_height}}.
#' @export
beamheight <- function(range, elev, k = 4/3, lat = 35, re = 6378, rp = 6357) {
  .Deprecated("beam_height")
  beam_height(range, elev, k, lat, re, rp)
}

#' @name beamwidth-deprecated
#' @rdname bioRad-deprecated
#' @section \code{beamwidth}:
#' For \code{beamwidth}, use \code{\link{beam_width}}.
#' @export
beamwidth <- function(range, angle = 1) {
  .Deprecated("beam_width")
  beam_width(range, angle)
}

#' @name bind-deprecated
#' @rdname bioRad-deprecated
#' @section \code{bind}:
#' For \code{bind}, use \code{\link{bind_into_vpts}}.
#' @export
bind <- function(x, ...) {
  .Deprecated("bind_into_vpts")
  bind_into_vpts(x, ...)
}

#' @name vol2bird-deprecated
#' @rdname bioRad-deprecated
#' @section \code{vol2bird}:
#' For \code{vol2bird}, use \code{\link{calculate_vp}}.
#' @export
vol2bird <- function(...) {
  .Deprecated("calculate_vp")
  calculate_vp(...)
}

#' @name night-deprecated
#' @rdname bioRad-deprecated
#' @section \code{night}:
#' For \code{night}, use \code{\link{check_night}}.
#' @export
night <- function(x, ..., elev=-0.268) {
  .Deprecated("check_night")
  check_night(x, ..., elev = -0.268)
}

#' @name day-deprecated
#' @rdname bioRad-deprecated
#' @section \code{day}:
#' For \code{day}, use \code{\link{check_night}}.
#' @export
day <- function(x, elev = -0.268) {
  .Deprecated("check_night",
              msg = paste("'day' is deprecatedand, its functionality is",
                          "replaced by the 'check_night' function",
                          "(FALSE <-> TRUE)"))
  !check_night(x, elev = -0.268)
}

#' @name composite-deprecated
#' @rdname bioRad-deprecated
#' @section \code{composite}:
#' For \code{composite}, use \code{\link{composite_ppi}}.
#' @export
composite <- function(...) {
  .Deprecated("composite_ppi")
  composite_ppi(...)
}

#' @name dbz2eta-deprecated
#' @rdname bioRad-deprecated
#' @section \code{dbz2eta}:
#' For \code{dbz2eta}, use \code{\link{dbz_to_eta}}.
#' @export
dbz2eta <- function(...) {
  .Deprecated("dbz_to_eta")
  dbz_to_eta(...)
}

#' @name eta2dbz-deprecated
#' @rdname bioRad-deprecated
#' @section \code{eta2dbz}:
#' For \code{eta2dbz}, use \code{\link{eta_to_dbz}}.
#' @export
eta2dbz <- function(...) {
  .Deprecated("eta_to_dbz")
  eta_to_dbz(...)
}

#' @name checkDocker-deprecated
#' @rdname bioRad-deprecated
#' @section \code{checkDocker}:
#' For \code{checkDocker}, use \code{\link{check_docker}}.
#' @export
checkDocker <- function(...) {
  .Deprecated("check_docker")
  check_docker(...)
}

#' @name updateDocker-deprecated
#' @rdname bioRad-deprecated
#' @section \code{updateDocker}:
#' For \code{updateDocker}, use \code{\link{update_docker}}.
#' @export
updateDocker <- function(...) {
  .Deprecated("update_docker")
  update_docker(...)
}

#' @name basemap-deprecated
#' @rdname bioRad-deprecated
#' @section \code{basemap}:
#' For \code{basemap}, use \code{\link{download_basemap}}.
#' @export
basemap <- function(...) {
  .Deprecated("download_basemap")
  download_basemap(...)
}

#' @name download_vp-deprecated
#' @rdname bioRad-deprecated
#' @section \code{download_vp}:
#' For \code{download_vp}, use \code{\link{download_vpfiles}}.
#' @export
download_vp <- function(...) {
  .Deprecated("download_vpfiles")
  download_vpfiles(...)
}

