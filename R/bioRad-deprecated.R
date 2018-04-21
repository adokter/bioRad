#' Deprecated functions in package \pkg{bioRad}
#'
#' The functions listed below are deprecated or renamed and will be defunct in
#' the near future. When possible, alternative functions with similar
#' functionality are mentioned.
#'
#' @name bioRad-deprecated
#' @keywords internal
NULL

#' @name beamheight-deprecated
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

#' @name beamwidth-deprecated
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

#' @name bind-deprecated
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

#' @name vol2bird-deprecated
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

#' @name night-deprecated
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

#' @name day-deprecated
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

#' @name composite-deprecated
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

#' @name dbz2eta-deprecated
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

#' @name eta2dbz-deprecated
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

#' @name checkDocker-deprecated
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

#' @name updateDocker-deprecated
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

#' @name basemap-deprecated
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

#' @name download_vp-deprecated
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

#' @name elangle-deprecated
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

#' @name fetch-deprecated
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

#' @name getscan-deprecated
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

#' @name vintegrate-deprecated
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

#' @name rsl2odim-deprecated
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

#' @name ppi-deprecated
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

#' @name h5ODIMobject-deprecated
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

#' @name read.pvol-deprecated
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

#' @name readvp-deprecated
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

#' @name readvp.list-deprecated
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

#' @name readvp.table-deprecated
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

#' @name regularize-deprecated
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

#' @name sd_vvp-deprecated
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

#' @name sd_vvp-deprecated
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

#' @name retrieve_vp_paths-deprecated
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

#' @name suntime-deprecated
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

#' @name is.vplist-deprecated
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

#' @name vpts-deprecated
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

#' @name VP-deprecated
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
#' @name VPTS-deprecated
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
#' @name SCAN-deprecated
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

#' Migration traffic rate
#'
#' Migration traffic rate (MTR) for an altitude layer, defined as the
#' number of targets crossing a 1 km line perpendicular to the migratory
#' movement per hour.
#'
#' @param x A \code{vp}, list of \code{vp} objects or \code{vpts} object.
#' @param alt.min Minimum altitude in m.
#' @param alt.max Maximum altitude in m.
#' @param alpha (optional) migratory direction of interest in clockwise degrees
#' from north, otherwise \code{NA}.
#'
#' @return An atomic vector of migration traffic rates in individuals/km/hour.
#'
#' @rdname bioRad-deprecated
#'
#' @export
#'
#' @details
#' Migration traffic rate (MTR) for an altitude layer is a flux measure, defined
#' as the number of targets crossing a unit of transect per hour.
#'
#' The transect direction is set by the angle \code{alpha}. When
#' \code{alpha=NA}, the transect runs perpendicular to the measured migratory
#' direction. \code{mtr} then equals the number of crossing targets per km
#' transect per hour, for a transect kept perpendicular to the measured
#' migratory movement at all times and altitudes. In this case \code{mtr} is
#' always a positive quantity, defined as:
#'
#' \deqn{mtr = \sum_i dens_i ff_i \Delta h}{mtr = \sum_i dens_i ff_i \Delta h}
#'
#' with the sum running over all altitude layers between \code{alt.min} and
#' \code{alt.max}, \eqn{dens_i} the bird density, \eqn{ff_i} the ground speed at
#' altitude layer i, and \eqn{\Delta h} the altitude layer width.
#'
#' If \code{alpha} is given a numeric value, the transect is taken perpendicular
#' to the direction \code{alpha}, and the number of crossing targets per hour
#' per km transect is calculated as:
#'
#' \deqn{mtr = \sum_i dens_i ff_i \cos(dd_i-alpha) \Delta h}{mtr = \sum_i dens_i ff_i \cos(dd_i-alpha) \Delta h}
#' with \eqn{dd_i} the migratory direction at altitude i.
#'
#' Note that this equation evaluates to the previous equation when \code{alpha} equals \eqn{dd_i}.
#'
#' In this definition \code{mtr} is a traditional flux into a direction of
#' interest. Targets moving into the direction \code{alpha} contribute
#' positively to \code{mtr}, while targets moving in the opposite direction
#' contribute negatively to \code{mtr}. Therefore \code{mtr} can be both
#' positive or negative, depending on the definition of alpha.
#'
#' This is a wrapper function for \link{integrate_profile}, extracting only the
#' migration traffic rate data.
#'
#' @examples
#' # MTR for a single vertical profile:
#' mtr(example_vp)
#'
#' # MTRs for a time series of vertical profiles:
#' data(example_vpts)
#' # print migration traffic rates:
#' mtr(example_vpts)
#' # to plot migration traffic rate data, use integrate_profile:
#' plot(integrate_profile(example_vpts), quantity = "mtr")
mtr <- function(x, alt.min = 0, alt.max = Inf, alpha = NA) {
  stopifnot(inherits(x, "vp") || inherits(x, "vpts") || inherits(x, "list"))
  if (inherits(x, "list")){
    vptest <- sapply(x, function(y) is(y, "vp"))
    if (FALSE %in% vptest) {
      stop("Not all objects in list are vp objects")
    }
  }
  .Deprecated("integrate_profile")
  .Deprecated(msg = "'mtr' has been moved to the 'mtr' column in the output of integrate_profile()")
  vintegrated <- integrate_profile(x, alt.min = alt.min, alt.max = alt.max,
                                   alpha = alpha)
  vintegrated$mtr
}

#' Migration traffic
#'
#' Total migration traffic, which is calculated by time-integration of
#' migration traffic rates. Migration traffic gives the number of individuals
#' that have passed per km perpendicular to the migratory direction at the
#' position of the radar for the full period of the time series within the
#' specified altitude band.
#'
#' @param x A \code{vpts}.
#' @param interval.max Maximum time interval belonging to a single profile in
#' seconds. Traffic rates are set to zero at times \code{t} for which no
#' profiles can be found within the period \code{t-interval.max/2} to
#' \code{t+interval.max/2}.
#'
#' @inheritParams mtr
#'
#' @return A numeric value equal to migration traffic in number of
#' individuals / km.
#'
#' @rdname bioRad-deprecated
#'
#' @export
#'
#' @examples
#' # get example time series of vertical profiles:
#' data(example_vpts)
#' example_vpts
#' # total migration traffic in full altitude band:
#' mt(example_vpts)
#' # total migration traffic in 0-1000 meter band:
#' mt(example_vpts, alt.min = 0, alt.max = 1000)
mt <- function(x, alt.min = 0, alt.max = Inf, alpha = NA, interval.max = Inf) {
  stopifnot(inherits(x,"vpts"))
  .Deprecated("integrate_profile")
  .Deprecated(msg = "'mt' has been moved to the 'mt' column in the output of integrate_profile()")
  cmt(x,alt.min,alt.max,alpha,interval.max)[ncol(x)]
}

#' Cumulative migration traffic
#'
#' Cumulative migration traffic is calculated as the cumulative sum of the
#' migration traffic within each time step of a time series. Cumulative
#' migration traffic gives the number of individuals that have passed per km
#' perpendicular to the migratory direction at the position of the radar as a
#' function of time from the start of time series within the specified altitude
#' band.
#'
#' @param x A \code{vpts}.
#' @param interval.max Maximum time interval belonging to a single profile in
#' seconds. Traffic rates are set to zero at times \code{t} for which no
#' profiles can be found within the period \code{t-interval.max/2} to
#' \code{t+interval.max/2}.
#'
#' @inheritParams mtr
#'
#' @return Atomic vector with (cumulative) migration traffic in number of
#' individuals / km.
#'
#' @rdname bioRad-deprecated
#'
#' @export
#'
#' @examples
#' # get example time series of vertical profiles:
#' data(example_vpts)
#' # print cumulative migration traffic to console:
#' cmt(example_vpts)
#' # plot cumulative migration traffic:
#' plot(cmt(example_vpts), type = "l", xlab = "time" , ylab = "CMT [birds/km]")
cmt <- function(x, alt.min = 0, alt.max = Inf, alpha = NA, interval.max = Inf) {
  stopifnot(inherits(x,"vpts"))
  .Deprecated("integrate_profile")
  .Deprecated(msg = "'cmt' has been moved to the 'mt' column in the output of integrate_profile()")
  vintegrated=integrate_profile(x,alt.min,alt.max,alpha,interval.max)
  vintegrated$mt
}
