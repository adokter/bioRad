#' Deprecated functions and data in package \pkg{bioRad}
#'
#' The functions and data listed below are deprecated or renamed and will be
#' defunct in the near future. When possible, alternative functions with similar
#' functionality are mentioned.
#'
#' @name bioRad-deprecated
#' @keywords internal
NULL

#' @section basemap:
#' Use \code{\link{download_basemap}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
basemap <- function(...) {
  .Deprecated("download_basemap")
  download_basemap(...)
}

#' @section beamheight:
#' Use \code{\link{beam_height}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
beamheight <- function(range, elev, k = 4 / 3, lat = 35, re = 6378, rp = 6357) {
  .Deprecated("beam_height")
  beam_height(range, elev, k, lat, re, rp)
}

#' @section beamwidth:
#' Use \code{\link{beam_width}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
beamwidth <- function(range, angle = 1) {
  .Deprecated("beam_width")
  beam_width(range, beam_angle = angle)
}

#' @section bind:
#' Use \code{\link{bind_into_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
bind <- function(x, ...) {
  .Deprecated("bind_into_vpts")
  bind_into_vpts(x, ...)
}

#' @section checkDocker:
#' Use \code{\link{check_docker}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
checkDocker <- function(...) {
  .Deprecated("check_docker")
  check_docker(...)
}

#' @section composite:
#' Use \code{\link{composite_ppi}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
composite <- function(x, param = "DBZH", cells.dim = c(100, 100)) {
  .Deprecated("composite_ppi")
  composite_ppi(x, param, dim = cells.dim)
}

#' @section day:
#' Use \code{\link{check_night}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
day <- function(x, ..., elev = -0.268) {
  .Deprecated("check_night",
    msg = paste(
      "'day' is deprecated and its functionality is",
      "replaced by the 'check_night' function",
      "(FALSE <-> TRUE)"
    )
  )
  !check_night(x, ..., elev = -0.268)
}

#' @section dbz2eta:
#' Use \code{\link{dbz_to_eta}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
dbz2eta <- function(dbz, wavelength, Km = 0.93) {
  .Deprecated("dbz_to_eta")
  dbz_to_eta(dbz, wavelength, K = Km)
}

#' @section download_vp:
#' Use \code{\link{download_vpfiles}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
download_vp <- function(start_date, end_date, country, radar, localpath = ".") {
  .Deprecated("download_vpfiles")
  download_vpfiles(
    date_min = start_date, date_max = end_date,
    country, radar, directory = localpath
  )
}

#' @section elangle:
#' Use \code{\link{get_elevation_angles}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
elangle <- function(...) {
  .Deprecated("get_elevation_angles")
  get_elevation_angles(...)
}

#' @section eta2dbz:
#' Use \code{\link{eta_to_dbz}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
eta2dbz <- function(eta, wavelength, Km = 0.93) {
  .Deprecated("eta_to_dbz")
  eta_to_dbz(eta, wavelength, K = Km)
}

#' @section fetch:
#' Use \code{\link{get_quantity}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
fetch <- function(...) {
  .Deprecated("get_quantity")
  get_quantity(...)
}

#' @section getscan:
#' Use \code{\link{get_scan}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
getscan <- function(x, angle) {
  .Deprecated("get_scan")
  get_scan(x, elev = angle)
}

#' @section h5ODIMobject:
#' Use \code{\link{get_odim_object_type}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
h5ODIMobject <- function(filename) {
  .Deprecated("get_odim_object_type")
  get_odim_object_type(file = filename)
}

#' @section is.vplist:
#' The \code{vplist} object is deprecated. Use a regular list of \code{vp}
#' objects (\code{c(vp, vp))} instead, which elements can be checked
#' individually with \code{\link{is.vp}}.
#'
#' @rdname bioRad-deprecated
#' @export
is.vplist <- function(...) {
  .Deprecated("is.vp")
}

#' @section mt:
#' \code{mt} is deprecated as a separate function. Migration traffic is now
#' included in the output of \code{\link{integrate_profile}} as column
#' \code{mt}.
#'
#' \strong{Deprecated description}
#'
#' Total migration traffic, which is calculated by time-integration of
#' migration traffic rates. Migration traffic gives the number of individuals
#' that have passed per km perpendicular to the migratory direction at the
#' position of the radar for the full period of the time series within the
#' specified altitude band.
#'
#' \strong{Deprecated examples}
#'
#' \preformatted{
#' # get example time series of vertical profiles:
#' data(example_vpts)
#' example_vpts
#' # total migration traffic in full altitude band:
#' mt(example_vpts)
#' # total migration traffic in 0-1000 meter band:
#' mt(example_vpts, alt.min = 0, alt.max = 1000)
#' }
#'
#' @rdname bioRad-deprecated
#' @export
mt <- function(x, alt.min = 0, alt.max = Inf, alpha = NA, interval.max = Inf) {
  .Deprecated("integrate_profile")
  .Deprecated(msg = paste(
    "Migration traffic is now included in the output",
    "of `integrate_profile()` as column 'mt'."
  ))
  stopifnot(inherits(x, "vpts"))
  cmt(x, alt.min, alt.max, alpha, interval.max)[ncol(x), 2]
}

#' @section mtr:
#' \code{\link{mtr}} is deprecated as a separate function. Migration traffic
#' rate is now included in the output of \code{\link{integrate_profile}} as
#' column \code{mtr}.
#'
#' \strong{Deprecated description}
#'
#' Migration traffic rate (MTR) for an altitude layer, defined as the number of
#' targets crossing a 1 km line perpendicular to the migratory movement per
#' hour.
#'
#' \strong{Deprecated details}
#'
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
#' \deqn{mtr = \sum_i dens_i ff_i \cos(dd_i-alpha) \Delta h}{mtr = \sum_i dens_i
#' ff_i \cos(dd_i-alpha) \Delta h} with \eqn{dd_i} the migratory direction at
#' altitude i.
#'
#' Note that this equation evaluates to the previous equation when \code{alpha}
#' equals \eqn{dd_i}.
#'
#' In this definition \code{mtr} is a traditional flux into a direction of
#' interest. Targets moving into the direction \code{alpha} contribute
#' positively to \code{mtr}, while targets moving in the opposite direction
#' contribute negatively to \code{mtr}. Therefore \code{mtr} can be both
#' positive or negative, depending on the definition of alpha.
#'
#' \strong{Deprecated examples}
#'
#' \preformatted{
#' # MTR for a single vertical profile:
#' mtr(example_vp)
#' # MTRs for a time series of vertical profiles:
#' data(example_vpts)
#' # print migration traffic rates:
#' mtr(example_vpts)
#' # to plot migration traffic rate data, use integrate_profile:
#' plot(integrate_profile(example_vpts), quantity = "mtr")
#' }
#'
#' @rdname bioRad-deprecated
#' @export
mtr <- function(x, alt.min = 0, alt.max = Inf, alpha = NA) {
  .Deprecated("integrate_profile")
  .Deprecated(msg = paste(
    "Migration traffic rate is now included in the",
    "output of `integrate_profile()` as column 'mtr'."
  ))
  stopifnot(inherits(x, "vp") || inherits(x, "vpts") || inherits(x, "list"))
  if (inherits(x, "list")) {
    vptest <- sapply(x, function(y) is(y, "vp"))
    if (FALSE %in% vptest) {
      stop("Not all objects in list are vp objects")
    }
  }
  vintegrated <- integrate_profile(x,
    alt_min = alt.min, alt_max = alt.max,
    alpha = alpha
  )
  vintegrated$mtr
}

#' @section cmt:
#' \code{\link{cmt}} is deprecated as a separate function. Cumulative migration
#' traffic is now included in the output of \code{\link{integrate_profile}} as
#' column \code{mt}, which can be summed to get the cumulative migration
#' traffic.
#'
#' \strong{Deprecated description}
#'
#' Cumulative migration traffic is calculated as the cumulative sum of the
#' migration traffic within each time step of a time series. Cumulative
#' migration traffic gives the number of individuals that have passed per km
#' perpendicular to the migratory direction at the position of the radar as a
#' function of time from the start of time series within the specified altitude
#' band.
#'
#' \strong{Deprecated examples}
#'
#' \preformatted{
#' # get example time series of vertical profiles:
#' data(example_vpts)
#'
#' # print cumulative migration traffic to console:
#' cmt(example_vpts)
#' }
#'
#' @rdname bioRad-deprecated
#' @export
cmt <- function(x, alt.min = 0, alt.max = Inf, alpha = NA, interval.max = Inf) {
  .Deprecated("integrate_profile")
  .Deprecated(msg = paste(
    "Cumulative migration traffic is now included in the",
    "output of `integrate_profile()` as column 'mt' (summed)."
  ))
  stopifnot(inherits(x, "vpts"))
  vintegrated <- integrate_profile(x,
    alt_min = alt.min, alt_max = alt.max,
    alpha = alpha, interval_max = interval.max
  )
  data.frame(datetime = vintegrated$datetime, mt = vintegrated$mt)
}

#' @section night:
#' Use \code{\link{check_night}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
night <- function(x, ..., elev = -0.268) {
  .Deprecated("check_night")
  check_night(x, ..., elev = -0.268)
}

#' @section ppi:
#' Use \code{\link{project_as_ppi}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
ppi <- function(x, cellsize = 500, range.max = 50000,
                project = FALSE, latlim = NULL, lonlim = NULL) {
  .Deprecated("project_as_ppi")
  project_as_ppi(x,
    grid_size = cellsize, range_max = range.max,
    project = project, ylim = latlim, xlim = lonlim
  )
}

#' @section read.pvol:
#' Use \code{\link{read_pvolfile}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
read.pvol <- function(filename, param = c(
                        "DBZH", "VRADH", "VRAD", "RHOHV",
                        "ZDR", "PHIDP", "CELL"
                      ),
                      sort = TRUE, lat, lon, height, elangle.min = 0,
                      elangle.max = 90, verbose = TRUE,
                      mount = dirname(filename)) {
  .Deprecated("read_pvolfile")
  read_pvolfile(
    file = filename, param = param, sort = sort, lat = lat,
    lon = lon, height = height, elev_min = elangle.min,
    elev_max = elangle.max, verbose = verbose, mount = mount
  )
}

#' @section readvp:
#' Use \code{\link{read_vpfiles}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
readvp <- function(...) {
  .Deprecated("read_vpfiles")
  read_vpfiles(...)
}

#' @section readvp.list:
#' Use \code{\link{read_vpfiles}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
readvp.list <- function(...) {
  .Deprecated("read_vpfiles")
  read_vpfiles(...)
}

#' @section readvp.table:
#' Use \code{\link{read_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
readvp.table <- function(...) {
  .Deprecated("read_vpts")
  read_vpts(...)
}

#' @section regularize:
#' Use \code{\link{regularize_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
regularize <- function(ts, interval = "auto", t.min = ts$daterange[1],
                       t.max = ts$daterange[2], units = "mins",
                       fill = FALSE, verbose = TRUE) {
  .Deprecated("regularize_vpts")
  regularize_vpts(ts, interval,
    date_min = t.min, date_max = t.max, units,
    fill, verbose
  )
}

#' @section retrieve_vp_paths:
#' Use \code{\link{select_vpfiles}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
retrieve_vp_paths <- function(path, start_date, end_date,
                              country = NULL, radar = NULL) {
  .Deprecated("select_vpfiles")
  select_vpfiles(
    directory = path, date_min = start_date, date_max = end_date,
    country, radar
  )
}

#' @section rsl2odim:
#' Use \code{\link{nexrad_to_odim}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
rsl2odim <- function(vol.in, vol.out, verbose = FALSE,
                     mount = dirname(vol.in)) {
  .Deprecated("nexrad_to_odim")
  nexrad_to_odim(
    pvolfile_nexrad = vol.in, pvolfile_odim = vol.out, verbose,
    mount
  )
}

#' @section sd_vvp:
#' Use \code{\link{sd_vvp_threshold}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
sd_vvp <- function(x) {
  .Deprecated("sd_vvp_threshold")
  sd_vvp_threshold(x)
}

#' @section suntime:
#' Use \code{\link{sunrise}} or \code{\link{sunset}} instead.
#'
#' @rdname bioRad-deprecated
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
#' @export
updateDocker <- function(...) {
  .Deprecated("update_docker")
  update_docker(...)
}

#' @section vintegrate:
#' Use \code{\link{integrate_profile}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
vintegrate <- function(...) {
  .Deprecated("integrate_profile")
  integrate_profile(...)
}

#' @section vol2bird:
#' Use \code{\link{calculate_vp}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
vol2bird <- function(vol.in, vp.out = "", vol.out = "", autoconf = FALSE,
                     verbose = FALSE, mount = dirname(vol.in),
                     sd_vvp_threshold = 2, rcs = 11, dualpol = FALSE,
                     rhohv = 0.95, elev.min = 0, elev.max = 90, azim.min = 0,
                     azim.max = 360, range.min = 5000, range.max = 25000,
                     nlayer = 20L, hlayer = 200, dealias = TRUE,
                     nyquist.min = if (dealias) 5 else 25,
                     dbz_quantity = "DBZH") {
  .Deprecated("calculate_vp")
  calculate_vp(
    pvolfile = vol.in, vpfile = vp.out, pvolfile_out = vol.out,
    autoconf = autoconf, verbose = verbose, mount = mount,
    sd_vvp_threshold = sd_vvp_threshold, rcs = rcs, dual_pol = dualpol,
    rho_hv = rhohv, elev_min = elev.min, elev_max = elev.max,
    azim_min = azim.min, azim_max = azim.max, range_min = range.min,
    range_max = range.max, n_layer = nlayer, h_layer = hlayer,
    dealias = dealias, nyquist_min = nyquist.min,
    dbz_quantity = dbz_quantity
  )
}

#' @section vpts:
#' Use \code{\link{bind_into_vpts}} instead.
#'
#' @rdname bioRad-deprecated
#' @export
vpts <- function(...) {
  .Deprecated("vplist_to_vpts")
  vplist_to_vpts(...)
}

#' @section \code{VP}:
#' Use \code{\link{example_vp}} instead.
#'
#' @name VP-deprecated
#' @rdname bioRad-deprecated
NULL

#' @section \code{VPTS}:
#' Use \code{\link{example_vpts}} instead.
#'
#' @name VPTS-deprecated
#' @rdname bioRad-deprecated
NULL

#' @section \code{SCAN}:
#' Use \code{\link{example_scan}} instead.
#'
#' @name SCAN-deprecated
#' @rdname bioRad-deprecated
NULL
