#' Deprecated functions in package \pkg{bioRad}
#'
#' The functions listed below are deprecated or renamed and will be defunct in
#' the near future. When possible, alternative functions with similar
#' functionality are mentioned.
#'
#' @name bioRad-deprecated
#' @keywords internal
NULL

# basemap
#' @rdname bioRad-deprecated
#'
#' @section \code{basemap}:
#' Use \code{\link{download_basemap}} instead.
#'
#' @export
basemap <- function(...) {
  .Deprecated("download_basemap")
  download_basemap(...)
}

# beamheight
#' @rdname bioRad-deprecated
#'
#' @section \code{beamheight}:
#' Use \code{\link{beam_height}} instead.
#'
#' @export
beamheight <- function(range, elev, k = 4/3, lat = 35, re = 6378, rp = 6357) {
  .Deprecated("beam_height")
  beam_height(range, elev, k, lat, re, rp)
}

# beamwidth
#' @rdname bioRad-deprecated
#'
#' @section \code{beamwidth}:
#' Use \code{\link{beam_width}} instead.
#'
#' @export
beamwidth <- function(range, angle = 1) {
  .Deprecated("beam_width")
  beam_width(range, angle)
}

# bind
#' @rdname bioRad-deprecated
#'
#' @section \code{bind}:
#' Use \code{\link{bind_into_vpts}} instead.
#'
#' @export
bind <- function(x, ...) {
  .Deprecated("bind_into_vpts")
  bind_into_vpts(x, ...)
}

# checkDocker
#' @rdname bioRad-deprecated
#'
#' @section \code{checkDocker}:
#' Use \code{\link{check_docker}} instead.
#'
#' @export
checkDocker <- function(...) {
  .Deprecated("check_docker")
  check_docker(...)
}

# composite
#' @rdname bioRad-deprecated
#'
#' @section \code{composite}:
#' Use \code{\link{composite_ppi}} instead.
#'
#' @export
composite <- function(...) {
  .Deprecated("composite_ppi")
  composite_ppi(...)
}

# cmt
#' @rdname bioRad-deprecated
#'
#' @section \code{cmt}:
#' \code{\link{cmt}} is deprecated as a separate function. Cumulative migration
#' traffic is now included in the output of \code{\link{integrate_profile}} as
#' column \code{mt}, which can be summed to get the cumulative migration
#' traffic.
#'
#' @export
cmt <- function(...) {
  cmt(...)
}

# day
#' @rdname bioRad-deprecated
#'
#' @section \code{day}:
#' Use \code{\link{check_night}} instead.
#'
#' @export
day <- function(x, elev = -0.268) {
  .Deprecated("check_night",
              msg = paste("'day' is deprecated and its functionality is",
                          "replaced by the 'check_night' function",
                          "(FALSE <-> TRUE)"))
  !check_night(x, elev = -0.268)
}

# dbz2eta
#' @rdname bioRad-deprecated
#'
#' @section \code{dbz2eta}:
#' Use \code{\link{dbz_to_eta}} instead.
#'
#' @export
dbz2eta <- function(...) {
  .Deprecated("dbz_to_eta")
  dbz_to_eta(...)
}

# download_vp
#' @rdname bioRad-deprecated
#'
#' @section \code{download_vp}:
#' Use \code{\link{download_vpfiles}} instead.
#'
#' @export
download_vp <- function(...) {
  .Deprecated("download_vpfiles")
  download_vpfiles(...)
}

# elangle
#' @rdname bioRad-deprecated
#'
#' @section \code{elangle}:
#' Use \code{\link{get_angles}} instead.
#'
#' @export
elangle <- function(...) {
  .Deprecated("get_angles")
  get_angles(...)
}

# eta2dbz
#' @rdname bioRad-deprecated
#'
#' @section \code{eta2dbz}:
#' Use \code{\link{eta_to_dbz}} instead.
#'
#' @export
eta2dbz <- function(...) {
  .Deprecated("eta_to_dbz")
  eta_to_dbz(...)
}

# fetch
#' @rdname bioRad-deprecated
#'
#' @section \code{fetch}:
#' Use \code{\link{get_quantity}} instead.
#'
#' @export
fetch <- function(...) {
  .Deprecated("get_quantity")
  get_quantity(...)
}

# getscan
#' @rdname bioRad-deprecated
#'
#' @section \code{getscan}:
#' Use \code{\link{get_scan}} instead.
#'
#' @export
getscan <- function(...) {
  .Deprecated("get_scan")
  get_scan(...)
}

# h5ODIMobject
#' @rdname bioRad-deprecated
#'
#' @section \code{h5ODIMobject}:
#' Use \code{\link{get_odim_object_type}} instead.
#'
#' @export
h5ODIMobject <- function(...) {
  .Deprecated("get_odim_object_type")
  get_odim_object_type(...)
}

# is.vplist
#' @rdname bioRad-deprecated
#'
#' @section \code{is.vplist}:
#' The \code{vplist} object is deprecated. Use a regular list of \code{vp}
#' objects (\code{c(vp, vp))} instead.
#'
#' @export
is.vplist <- function(...) {
  .Deprecated("is.vplist")
}

# mt
#' @rdname bioRad-deprecated
#'
#' @section \code{mt}:
#' \code{\link{mt}} is deprecated as a separate function. Migration traffic
#' is now included in the output of \code{\link{integrate_profile}} as
#' column \code{mt}.
#'
#' @export
mt <- function(...) {
  mt(...)
}

# mtr
#' @rdname bioRad-deprecated
#'
#' @section \code{mtr}:
#' \code{\link{mtr}} is deprecated as a separate function. Migration traffic
#' rate is now included in the output of \code{\link{integrate_profile}} as
#' column \code{mtr}.
#'
#' @export
mtr <- function(...) {
  mtr(...)
}

# night
#' @rdname bioRad-deprecated
#'
#' @section \code{night}:
#' Use \code{\link{check_night}} instead.
#'
#' @export
night <- function(x, ..., elev=-0.268) {
  .Deprecated("check_night")
  check_night(x, ..., elev = -0.268)
}

# ppi
#' @rdname bioRad-deprecated
#'
#' @section \code{ppi}:
#' Use \code{\link{project_as_ppi}} instead.
#'
#' @export
ppi <- function(...) {
  .Deprecated("project_as_ppi")
  project_as_ppi(...)
}

# read.pvol
#' @rdname bioRad-deprecated
#'
#' @section \code{read.pvol}:
#' Use \code{\link{read_pvolfile}} instead.
#'
#' @export
read.pvol <- function(...) {
  .Deprecated("read_pvolfile")
  read_pvolfile(...)
}

# readvp
#' @rdname bioRad-deprecated
#'
#' @section \code{readvp}:
#' Use \code{\link{read_vpfiles}} instead.
#'
#' @export
readvp <- function(...) {
  .Deprecated("read_vpfiles")
  read_vpfiles(...)
}

# readvp.list
#' @rdname bioRad-deprecated
#'
#' @section \code{readvp.list}:
#' Use \code{\link{read_vpfiles}} instead.
#'
#' @export
readvp.list <- function(...) {
  .Deprecated("read_vpfiles")
  read_vpfiles(...)
}

# readvp.table
#' @rdname bioRad-deprecated
#'
#' @section \code{readvp.table}:
#' Use \code{\link{read_vpts}} instead.
#'
#' @export
readvp.table <- function(...) {
  .Deprecated("read_vpts")
  read_vpts(...)
}

# regularize
#' @rdname bioRad-deprecated
#'
#' @section \code{regularize}:
#' Use \code{\link{regularize_vpts}} instead.
#'
#' @export
regularize <- function(...) {
  .Deprecated("regularize_vpts")
  regularize_vpts(...)
}

# retrieve_vp_paths
#' @rdname bioRad-deprecated
#'
#' @section \code{retrieve_vp_paths}:
#' Use \code{\link{select_vpfiles}} instead.
#'
#' @export
retrieve_vp_paths <- function(...) {
  .Deprecated("select_vpfiles")
  select_vpfiles(...)
}

# rsl2odim
#' @rdname bioRad-deprecated
#'
#' @section \code{rsl2odim}:
#' Use \code{\link{nexrad_to_odim}} instead.
#'
#' @export
rsl2odim <- function(...) {
  .Deprecated("nexrad_to_odim")
  nexrad_to_odim(...)
}

# sd_vvp
#' @rdname bioRad-deprecated
#'
#' @section \code{sd_vvp}:
#' Use \code{\link{rvsd}} instead.
#'
#' @export
sd_vvp <- function(...) {
  .Deprecated("rvsd")
  rvsd(...)
}

# sd_vvp<-
#' @rdname bioRad-deprecated
#'
#' @section \code{sd_vvp<-}:
#' Use \code{\link{rvsd<-}} instead.
#'
#' @export
`sd_vvp<-` <- function(x, value) {
  .Deprecated("rvsd<-")
  `rvsd<-`(x, value)
}

# suntime
#' @rdname bioRad-deprecated
#'
#' @section \code{suntime}:
#' Use \code{\link{sunrise}} or \code{\link{sunset}} instead.
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

# updateDocker
#' @rdname bioRad-deprecated
#'
#' @section \code{updateDocker}:
#' Use \code{\link{update_docker}} instead.
#'
#' @export
updateDocker <- function(...) {
  .Deprecated("update_docker")
  update_docker(...)
}

# vintegrate
#' @rdname bioRad-deprecated
#'
#' @section \code{vintegrate}:
#' Use \code{\link{integrate_profile}} instead.
#'
#' @export
vintegrate <- function(...) {
  .Deprecated("integrate_profile")
  integrate_profile(...)
}

# vol2bird
#' @rdname bioRad-deprecated
#'
#' @section \code{vol2bird}:
#' Use \code{\link{calculate_vp}} instead.
#'
#' @export
vol2bird <- function(...) {
  .Deprecated("calculate_vp")
  calculate_vp(...)
}

# vpts
#' @rdname bioRad-deprecated
#'
#' @section \code{vpts}:
#' Use \code{\link{bind_to_vpts}} instead.
#'
#' @export
vpts <- function(...) {
  .Deprecated("vplist_to_vpts")
  vplist_to_vpts(...)
}

# deprecated example files

# SCAN
#' @rdname bioRad-deprecated
#'
#' @section \code{SCAN}:
#' Use \code{\link{example_scan}} instead.
#'
#' @export
SCAN <- function(...) {
  .Deprecated("example_scan")
  example_scan(...)
}

# VP
#' @rdname bioRad-deprecated
#'
#' @section \code{VP}:
#' Use \code{\link{example_vp}} instead.
#'
#' @export
VP <- function(...) {
  .Deprecated("example_vp")
  example_vp(...)
}

# VPTS
#' @rdname bioRad-deprecated
#'
#' @section \code{VPTS}:
#' Use \code{\link{example_vpts}} instead.
#'
#' @export
VPTS <- function(...) {
  .Deprecated("example_vpts")
  example_vpts(...)
}
