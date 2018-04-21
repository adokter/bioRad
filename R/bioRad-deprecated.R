#' Deprecated functions in package \pkg{bioRad}
#'
#' The functions listed below are deprecated or renamed and will be defunct in
#' the near future. When possible, alternative functions with similar
#' functionality are mentioned.
#'
#' @name bioRad-deprecated
#' @keywords internal
NULL

#' @section basemap:
#' Use \code{\link{download_basemap}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
basemap <- function(...) {
  .Deprecated("download_basemap")
  download_basemap(...)
}

#' @section beamheight:
#' Use \code{\link{beam_height}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
beamheight <- function(range, elev, k = 4/3, lat = 35, re = 6378, rp = 6357) {
  .Deprecated("beam_height")
  beam_height(range, elev, k, lat, re, rp)
}

#' @section beamwidth:
#' Use \code{\link{beam_width}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
beamwidth <- function(range, angle = 1) {
  .Deprecated("beam_width")
  beam_width(range, angle)
}

#' @section bind:
#' Use \code{\link{bind_into_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
bind <- function(x, ...) {
  .Deprecated("bind_into_vpts")
  bind_into_vpts(x, ...)
}

#' @section checkDocker:
#' Use \code{\link{check_docker}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
checkDocker <- function(...) {
  .Deprecated("check_docker")
  check_docker(...)
}

#' @section composite:
#' Use \code{\link{composite_ppi}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
composite <- function(...) {
  .Deprecated("composite_ppi")
  composite_ppi(...)
}

#' @section cmt:
#' \code{\link{cmt}} is deprecated as a separate function. Cumulative migration
#' traffic is now included in the output of \code{\link{integrate_profile}} as
#' column \code{mt}, which can be summed to get the cumulative migration
#' traffic.
#'
#' @rdname bioRad-deprecated
#'
#' @export
cmt <- function(...) {
  cmt(...)
}

#' @section day:
#' Use \code{\link{check_night}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
day <- function(x, elev = -0.268) {
  .Deprecated("check_night",
              msg = paste("'day' is deprecated and its functionality is",
                          "replaced by the 'check_night' function",
                          "(FALSE <-> TRUE)"))
  !check_night(x, elev = -0.268)
}

#' @section dbz2eta:
#' Use \code{\link{dbz_to_eta}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
dbz2eta <- function(...) {
  .Deprecated("dbz_to_eta")
  dbz_to_eta(...)
}

#' @section download_vp:
#' Use \code{\link{download_vpfiles}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
download_vp <- function(...) {
  .Deprecated("download_vpfiles")
  download_vpfiles(...)
}

#' @section elangle:
#' Use \code{\link{get_angles}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
elangle <- function(...) {
  .Deprecated("get_angles")
  get_angles(...)
}

#' @section eta2dbz:
#' Use \code{\link{eta_to_dbz}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
eta2dbz <- function(...) {
  .Deprecated("eta_to_dbz")
  eta_to_dbz(...)
}

#' @section fetch:
#' Use \code{\link{get_quantity}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
fetch <- function(...) {
  .Deprecated("get_quantity")
  get_quantity(...)
}

#' @section getscan:
#' Use \code{\link{get_scan}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
getscan <- function(...) {
  .Deprecated("get_scan")
  get_scan(...)
}

#' @section h5ODIMobject:
#' Use \code{\link{get_odim_object_type}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
h5ODIMobject <- function(...) {
  .Deprecated("get_odim_object_type")
  get_odim_object_type(...)
}

#' @section is.vplist:
#' The \code{vplist} object is deprecated. Use a regular list of \code{vp}
#' objects (\code{c(vp, vp))} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
is.vplist <- function(...) {
  .Deprecated("is.vplist")
}

#' @section mt:
#' \code{\link{mt}} is deprecated as a separate function. Migration traffic
#' is now included in the output of \code{\link{integrate_profile}} as
#' column \code{mt}.
#'
#' @rdname bioRad-deprecated
#'
#' @export
mt <- function(...) {
  mt(...)
}

#' @section mtr:
#' \code{\link{mtr}} is deprecated as a separate function. Migration traffic
#' rate is now included in the output of \code{\link{integrate_profile}} as
#' column \code{mtr}.
#'
#' @rdname bioRad-deprecated
#'
#' @export
mtr <- function(...) {
  mtr(...)
}

#' @section night:
#' Use \code{\link{check_night}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
night <- function(x, ..., elev=-0.268) {
  .Deprecated("check_night")
  check_night(x, ..., elev = -0.268)
}

#' @section ppi:
#' Use \code{\link{project_as_ppi}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
ppi <- function(...) {
  .Deprecated("project_as_ppi")
  project_as_ppi(...)
}

#' @section read.pvol:
#' Use \code{\link{read_pvolfile}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
read.pvol <- function(...) {
  .Deprecated("read_pvolfile")
  read_pvolfile(...)
}

#' @section readvp:
#' Use \code{\link{read_vpfiles}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
readvp <- function(...) {
  .Deprecated("read_vpfiles")
  read_vpfiles(...)
}

#' @section readvp.list:
#' Use \code{\link{read_vpfiles}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
readvp.list <- function(...) {
  .Deprecated("read_vpfiles")
  read_vpfiles(...)
}

#' @section readvp.table:
#' Use \code{\link{read_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
readvp.table <- function(...) {
  .Deprecated("read_vpts")
  read_vpts(...)
}

#' @section regularize:
#' Use \code{\link{regularize_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
regularize <- function(...) {
  .Deprecated("regularize_vpts")
  regularize_vpts(...)
}

#' @section retrieve_vp_paths:
#' Use \code{\link{select_vpfiles}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
retrieve_vp_paths <- function(...) {
  .Deprecated("select_vpfiles")
  select_vpfiles(...)
}

#' @section rsl2odim:
#' Use \code{\link{nexrad_to_odim}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
rsl2odim <- function(...) {
  .Deprecated("nexrad_to_odim")
  nexrad_to_odim(...)
}

#' @section sd_vvp:
#' Use \code{\link{rvsd}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
sd_vvp <- function(...) {
  .Deprecated("rvsd")
  rvsd(...)
}

#' @section sd_vvp<-:
#' Use \code{\link{rvsd<-}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
`sd_vvp<-` <- function(x, value) {
  .Deprecated("rvsd<-")
  `rvsd<-`(x, value)
}

#' @section suntime:
#' Use \code{\link{sunrise}} or \code{\link{sunset}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
suntime <- function(..., rise = TRUE) {
  if (rise == TRUE) {
    .Deprecated("sunrise")
    sunrise(...)
  } else {
    .Deprecated("sunset")
    sunset(...)
  }
}

#' @section updateDocker:
#' Use \code{\link{update_docker}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
updateDocker <- function(...) {
  .Deprecated("update_docker")
  update_docker(...)
}

#' @section vintegrate:
#' Use \code{\link{integrate_profile}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
vintegrate <- function(...) {
  .Deprecated("integrate_profile")
  integrate_profile(...)
}

#' @section vol2bird:
#' Use \code{\link{calculate_vp}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
vol2bird <- function(...) {
  .Deprecated("calculate_vp")
  calculate_vp(...)
}

#' @section vpts:
#' Use \code{\link{bind_into_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
vpts <- function(...) {
  .Deprecated("vplist_to_vpts")
  vplist_to_vpts(...)
}

# deprecated example files

#' @section SCAN:
#' Use \code{\link{example_scan}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
SCAN <- function(...) {
  .Deprecated("example_scan")
  example_scan(...)
}

#' @section VP:
#' Use \code{\link{example_vp}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
VP <- function(...) {
  .Deprecated("example_vp")
  example_vp(...)
}

#' @section VPTS:
#' Use \code{\link{example_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#'
#' @export
VPTS <- function(...) {
  .Deprecated("example_vpts")
  example_vpts(...)
}
