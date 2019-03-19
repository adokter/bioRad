#' Calculate a vertical profile (\code{vp}) from a polar volume (\code{pvol})
#'
#' Calculates a vertical profile of biological scatterers (vp) from a polar volume (pvol)
#' using the algorithm \href{https://github.com/adokter/vol2bird/}{vol2bird} (Dokter et al. 2011).
#'
#' @param pvolfile A radar file containing a radar polar volume, either in
#' \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM}
#' format, which is the implementation of the OPERA data information model in
#' \href{https://support.hdfgroup.org/HDF5/}{HDF5} format, or a format
#' supported by the
#' \href{http://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/}{RSL library}.
#' @param vpfile character. Filename for the vertical profile to be
#' generated in ODIM HDF5 format (optional).
#' @param pvolfile_out character. Filename for the polar volume to be
#' generated in ODIM HDF5 format (optional, e.g. for converting RSL formats
#' to ODIM).
#' @param autoconf logical. When TRUE, default optimal configuration settings
#' are selected automatically, and other user settings are ignored.
#' @param verbose logical. When TRUE, pipe Docker stdout to R console. On
#' Windows always TRUE.
#' @param mount character. String with the mount point (a directory path) for
#' the Docker container.
#' @param sd_vvp_threshold numeric. Lower threshold in radial velocity standard
#' deviation (profile quantity \code{sd_vvp}) in m/s. Biological signals with
#' \code{sd_vvp} < \code{sd_vvp_threshold} are set to zero.
#' @param rcs numeric. Radar cross section per bird in cm^2.
#' @param dual_pol logical. When \code{TRUE} use dual-pol mode, in which
#' meteorological echoes are filtered using the correlation coefficient
#' \code{rho_hv}. When \code{FALSE} use single polarization mode based only
#' on reflectivity and radial velocity quantities.
#' @param rho_hv numeric. Lower threshold in correlation coefficient used to
#' filter meteorological scattering.
#' @param elev_min numeric. Minimum scan elevation in degrees.
#' @param elev_max numeric. Maximum scan elevation in degrees.
#' @param azim_min numeric. Minimum azimuth in degrees clockwise from north.
#' @param azim_max numeric. Maximum azimuth in degrees clockwise from north.
#' @param range_min numeric. Minimum range in m.
#' @param range_max numeric. Maximum range in m.
#' @param n_layer numeric. Number of altitude layers in the profile.
#' @param h_layer numeric. Width of altitude layers in meter.
#' @param nyquist_min numeric. Minimum Nyquist velocity of scans in m/s for
#' scans to be included in the analysis.
#' @param dealias logical. Whether to dealias radial velocities; this should
#' typically be done when the scans in the polar volume have low Nyquist
#' velocities (below 25 m/s).
#' @param dbz_quantity character. One of the available reflectivity factor
#' quantities in the ODIM radar data format, e.g. DBZH, DBZV, TH, TV.
#'
#' @return A vertical profile object of class \link[=summary.vp]{vp}. When
#' defined, output files \code{vpfile} and \code{pvolfile_out} are saved to disk.
#'
#' @export
#'
#' @details Requires a running \href{https://www.docker.com/}{Docker} daemon.
#'
#' Common arguments set by users are \code{pvolfile}, \code{vpfile},
#' \code{autoconf} and \code{mount}.
#'
#' Turn on \code{autoconf} to automatically select the optimal parameters for a
#' given radar file. The default for C-band data is to apply rain-filtering in
#' single polarization mode, as well as dual polarization mode when available.
#'
#' The default for S-band data is to apply precipitation filtering in
#' dual-polarization mode.
#'
#' Arguments that sometimes require non-default values are: \code{rcs},
#' \code{sd_vvp_threshold}, \code{range_max}, \code{dual_pol}, \code{dealias}.
#'
#' Other arguments are typically left at their defaults.
#'
#' \code{azim_min} and \code{azim_max} only affects reflectivity-derived
#' estimates in the profile (DBZH,eta,dens), not radial-velocity derived
#' estimates (u, v, w, ff, dd, sd_vvp), which are estimated on all azimuths at
#' all times. \code{azim_min}, \code{azim_max} may be set to exclude an angular
#' sector with high ground clutter.
#'
#' \code{range_max} may be extended up to 40,000 m for volumes with low
#' elevations only, in order to extend coverage to higher altitudes.
#'
#' For altitude layers with a VVP-retrieved radial velocity standard deviation
#' value below the threshold \code{sd_vvp_threshold}, the bird density \code{dens} is set
#' to zero (see vertical profile \link[=summary.vp]{vp} class). This threshold
#' might be dependent on radar processing settings. Results from validation
#' campaigns so far indicate that 2 m/s is the best choice for this parameter
#' for most weather radars.

#' The algorithm has been tested and developed for altitude layers with
#' \code{h_layer} = 200 m. Smaller widths are not recommended as they may cause
#' instabilities of the volume velocity profiling (VVP) and dealiasing routines,
#' and effectively lead to pseudo-replicated altitude data, since altitudinal
#' patterns smaller than the beam width cannot be resolved.
#'
#' The default radar cross section (11 cm^2) corresponds to the average value
#' found by Dokter et al. in a calibration campaign of a full migration autumn
#' season in western Europe at C-band. It's value may depend on radar
#' wavelength. \code{rcs} will scale approximately \eqn{M^{2/3}} with \code{M}
#' the bird's mass.
#'
#' Using default values of \code{range_min} and \code{range_max} is
#' recommended. Ranges closer than 5 km tend to be contaminated by ground
#' clutter, while range gates beyond 25 km become too wide to resolve the
#' default altitude layer width of 200 meter (see \link{beam_width}).
#'
#' For dealiasing, the torus mapping method by Haase et al. is used.
#'
#' At S-band (radar wavelength ~ 10 cm), currently only \code{dual_pol=TRUE}
#' mode is recommended.
#'
#' On repeated calls of \code{calculate_vp}, the Docker container mount can be
#' recycled from one call to the next if subsequent calls share the same
#' \code{mount} argument. Re-mounting a Docker container takes time, therefore
#' it is advised to choose a mountpoint that is a parent directory of all
#' volume files to be processed, such that \code{calculate_vp} calls are as fast
#' as possible.
#'
#' @references
#' \itemize{
#'   \item Haase, G. and Landelius, T., 2004. Dealiasing of Doppler radar
#'   velocities using a torus mapping. Journal of Atmospheric and Oceanic
#'   Technology, 21(10), pp.1566-1573.
#'   \item Bird migration flight altitudes studied by a network of
#'   operational weather radars, Dokter et al., J. R. Soc. Interface 8 (54),
#'   pp. 30--43, 2011. \url{https://doi.org/10.1098/rsif.2010.0116}
#' }
#'
#' @examples
#' # locate example polar volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' 
#' # copy to a home directory with read/write permissions:
#' file.copy(pvolfile, "~/volume.h5")
#' 
#' # calculate the profile:
#' \dontrun{
#' profile <- calculate_vp("~/volume.h5")
#' }
#' 
#' # clean up:
#' file.remove("~/volume.h5")
calculate_vp <- function(pvolfile, vpfile = "", pvolfile_out = "",
                         autoconf = FALSE, verbose = FALSE,
                         mount = dirname(pvolfile), sd_vvp_threshold = 2,
                         rcs = 11, dual_pol = FALSE, rho_hv = 0.95, elev_min = 0,
                         elev_max = 90, azim_min = 0, azim_max = 360,
                         range_min = 5000, range_max = 25000, n_layer = 20L,
                         h_layer = 200, dealias = TRUE,
                         nyquist_min = if (dealias) 5 else 25,
                         dbz_quantity = "DBZH") {
  # check input arguments
  if (!file.exists(pvolfile)) {
    stop("No such file or directory")
  }
  if (!is.numeric(sd_vvp_threshold) || sd_vvp_threshold <= 0) {
    stop(
      "invalid 'sd_vvp_threshold' argument, radial velocity standard deviation ",
      "threshold should be a positive numeric value"
    )
  }
  if (!is.numeric(rcs) || rcs <= 0) {
    stop(
      "invalid 'rcs' argument, radar cross section should be a ",
      "positive numeric value"
    )
  }
  if (!is.logical(dual_pol)) {
    stop("invalid 'dual_pol' argument, should be logical")
  }
  if (!is.numeric(rho_hv) || rho_hv <= 0 || rho_hv > 1) {
    stop(
      "invalid 'rho_hv' argument, correlation coefficient treshold ",
      "should be a numeric value between 0 and 1"
    )
  }
  if (!is.numeric(elev_min) || elev_min < -90 || elev_min > 90) {
    stop(
      "invalid 'elev_min' argument, elevation should be between ",
      "-90 and 90 degrees"
    )
  }
  if (!is.numeric(elev_max) || elev_max < -90 || elev_max > 90) {
    stop(
      "invalid 'elev_max' argument, elevation should be between ",
      "-90 and 90 degrees"
    )
  }
  if (elev_max < elev_min) {
    stop("'elev_max' cannot be larger than 'elev_min'")
  }
  if (!is.numeric(azim_min) || azim_min < 0 || azim_min > 360) {
    stop(
      "invalid 'azim_min' argument, azimuth should be between ",
      "0 and 360 degrees"
    )
  }
  if (!is.numeric(azim_max) || azim_max < 0 || azim_max > 360) {
    stop(
      "invalid 'azim_max' argument, azimuth should be between ",
      "0 and 360 degrees"
    )
  }
  if (!is.numeric(range_min) || range_min < 0) {
    stop(
      "invalid 'range_min' argument, range should be a positive ",
      "numeric value"
    )
  }
  if (!is.numeric(range_max) || range_max < 0) {
    stop(
      "invalid 'range_max' argument, range should be a positive ",
      "numeric value"
    )
  }
  if (range_max < range_min) {
    stop("'rang.max' cannot be larger than 'rang.min'")
  }
  if (!is.integer(n_layer) & n_layer <= 0) {
    stop("'n_layer' should be a positive integer")
  }
  if (!is.numeric(h_layer) || h_layer < 0) {
    stop("invalid 'h_layer' argument, should be a positive numeric value")
  }
  if (!is.numeric(nyquist_min) || nyquist_min < 0) {
    stop("invalid 'nyquist_min' argument, should be a positive numeric value")
  }
  if (!(dbz_quantity %in% c("DBZ", "DBZH", "DBZV", "TH", "TV"))) {
    warning(paste("expecting 'dbz_quantity' to be one of DBZ, DBZH, DBZV, TH, TV"))
  }
  if (!is.logical(dealias)) {
    stop("invalid 'dealias' argument, should be logical")
  }
  if (file.access(mount, 0) == -1) {
    stop("invalid 'mount' argument. Directory not found")
  }
  if (file.access(mount, 2) == -1) {
    stop(paste(
      "invalid 'mount' argument. No write permission in directory",
      mount
    ))
  }
  if (!.pkgenv$docker) {
    stop(
      "Requires a running Docker daemon.\nTo enable calculate_vp, start ",
      "your local Docker daemon, and run 'check_docker()' in R\n"
    )
  }
  if (!length(autoconf) == 1 || !is.logical(autoconf)) {
    stop("autoconf argument should be one of TRUE or FALSE")
  }
  if (!length(verbose) == 1 || !is.logical(verbose)) {
    stop("verbose argument should be one of TRUE or FALSE")
  }
  if (vpfile != "" && !file.exists(dirname(vpfile))) {
    stop(paste("output directory", dirname(vpfile), "not found"))
  }

  filedir <- dirname(normalizePath(pvolfile, winslash = "/"))
  if (!grepl(normalizePath(mount, winslash = "/"), filedir, fixed = TRUE)) {
    stop(
      "mountpoint 'mount' has to be a parent directory ",
      "of input file 'pvolfile'"
    )
  }

  profile.tmp <- tempfile(tmpdir = filedir)
  if (file.access(filedir, mode = 2) < 0) {
    stop(paste("vol2bird requires write permission in", filedir))
  }
  if (mount_docker_container(normalizePath(mount, winslash = "/")) != 0) {
    stop(paste("failed to start vol2bird Docker container"))
  }

  # put options file in place, to be read by vol2bird container
  opt.values <- c(
    as.character(c(
      sd_vvp_threshold, rcs, rho_hv, elev_min, elev_max,
      azim_min, azim_max, range_min, range_max,
      n_layer, h_layer, nyquist_min, dbz_quantity
    )),
    if (dual_pol) "TRUE" else "FALSE",
    if (dealias) "TRUE" else "FALSE"
  )

  opt.names <- c(
    "STDEV_BIRD", "SIGMA_BIRD", "RHOHVMIN", "ELEVMIN", "ELEVMAX",
    "AZIMMIN", "AZIMMAX", "RANGEMIN", "RANGEMAX", "NLAYER",
    "HLAYER", "MIN_NYQUIST_VELOCITY", "DBZTYPE", "DUALPOL",
    "DEALIAS_VRAD"
  )
  opt <- data.frame(
    "option" = opt.names, "is" = rep("=", length(opt.values)),
    "value" = opt.values
  )
  optfile <- paste(normalizePath(mount, winslash = "/"),
    "/options.conf",
    sep = ""
  )

  if (file.exists(optfile)) {
    warning(paste("options.conf file found in directory ", mount,
      ". Renamed to options.conf.save to prevent overwrite...",
      sep = ""
    ))
    file.rename(optfile, paste(optfile, ".saved", sep = ""))
  }

  # only use user configuration when autoconfiguration is off.
  if (!autoconf) {
    write.table(opt,
      file = optfile, col.names = FALSE,
      row.names = FALSE, quote = FALSE
    )
  }

  # prepare docker input filenames relative to mountpoint
  prefixstart <- if (mount == "/") 1 else 2
  prefix <- substring(
    filedir,
    prefixstart + nchar(normalizePath(mount, winslash = "/"))
  )
  if (nchar(prefix) > 0) {
    prefix <- paste(prefix, "/", sep = "")
  }
  pvolfile_docker <- paste(prefix, basename(pvolfile), sep = "")
  profile.tmp.docker <- paste(prefix, basename(profile.tmp), sep = "")
  if (pvolfile_out != "") {
    pvolfile_out_docker <- paste(prefix, basename(pvolfile_out), sep = "")
  } else {
    pvolfile_out_docker <- ""
  }

  # run vol2bird container
  if (.Platform$OS.type == "unix") {
    result <- system(paste(
      "docker exec vol2bird bash -c \"cd data && vol2bird ",
      pvolfile_docker, profile.tmp.docker,
      pvolfile_out_docker, "\""
    ),
    ignore.stdout = !verbose
    )
  } else {
    winstring <- paste(
      "docker exec vol2bird bash -c \"cd data && vol2bird ",
      pvolfile_docker, profile.tmp.docker,
      pvolfile_out_docker, "\""
    )
    result <- suppressWarnings(system(winstring))
  }
  if (result != 0) {
    if (file.exists(optfile)) file.remove(optfile)
    stop("failed to run vol2bird Docker container")
  }

  # read output into a vp object
  output <- read_vpfiles(profile.tmp)

  # clean up
  if (vpfile == "") {
    file.remove(profile.tmp)
  } else {
    file.rename(profile.tmp, vpfile)
  }
  if (file.exists(optfile)) {
    file.remove(optfile)
  }

  output
}
