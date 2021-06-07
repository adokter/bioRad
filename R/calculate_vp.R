#' Calculate a vertical profile (\code{vp}) from a polar volume (\code{pvol})
#'
#' Calculates a vertical profile of biological scatterers (vp) from a polar volume (pvol)
#' using the algorithm \href{https://github.com/adokter/vol2bird/}{vol2bird} (Dokter et al. 2011).
#'
#' @param file character. String or a vector of strings with path(s) to radar file(s) for a radar polar volume.
#' Provide either a single file containing a polar volume, or multiple files with single scans/sweeps.
#' Data format should be either
#' \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM}
#' format, which is the implementation of the OPERA data information model in
#' \href{https://support.hdfgroup.org/HDF5/}{HDF5} format, or a format
#' supported by the
#' \href{http://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/}{RSL library}, or Vaisala IRIS (IRIS RAW) format.
#'
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
#' \code{sd_vvp} < \code{sd_vvp_threshold} are set to zero. Defaults to 2 m/s
#' for C-band radars and 1 m/s for S-band radars if not specified.
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
#' @param mistnet logical. Whether to use MistNet segmentation model.
#' @param mistnet_elevations numeric vector of length 5.
#' Elevation angles to feed to the MistNet
#' segmentation model, which expects exactly 5 elevation scans
#' at 0.5, 1.5, 2.5, 3.5 and 4.5 degrees. Specifying different
#' elevation angles may compromise segmentation results.
#' @param local_install character. String with path to local vol2bird installation
#'  (e.g. \code{"/your/vol2bird_install_directory/vol2bird/bin/vol2bird"}).
#'  To use local installation instead of Docker container, see details.
#' @param local_mistnet character. String with path to local mistnet segmentation model
#' in PyTorch format (e.g. \code{"/your/path/mistnet_nexrad.pt"}),
#' to use local installation instead of Docker container.
#' @param pvolfile character. Deprecated argument renamed to \code{file}.
#'
#' @return A vertical profile object of class \link[=summary.vp]{vp}. When
#' defined, output files \code{vpfile} and \code{pvolfile_out} are saved to disk.
#'
#' @export
#'
#' @details Requires a running \href{https://www.docker.com/}{Docker} daemon
#' (unless a local installation of vol2bird is specified with \code{local_install}).
#'
#' Common arguments set by users are \code{file}, \code{vpfile},
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
#' for most C-band weather radars, which is used as the C-band default. For S-band,
#' the default threshold is 1 m/s.

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
#' clutter, while range gates beyond 35 km become too wide to resolve the
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
#' If you have installed the vol2bird algorithm locally (not possible on Windows)
#' you can call vol2bird through this local installation (bypassing the Docker container),
#' which will be faster. Simply point \code{local_install} to the path
#' of your local vol2bird executable, e.g. {"/your/vol2bird_install_directory/vol2bird/bin/vol2bird"}.
#' Your local vol2bird executable will be called
#' through a bash login shell. LD_LIBRARY_PATH (Linux) or DYLD_LIBRARY_PATH (Mac) should be
#' correctly specified in your .bashrc or .bash_profile file
#' and contain all the required shared libraries by vol2bird. See vol2bird installation
#' pages on Github for details.
#'
#' When using MistNet with a local vol2bird installation, also point parameter \code{local_mistnet}
#' to your local download of the MistNet segmentation model in PyTorch format,
#' e.g. \code{"/your/path/mistnet_nexrad.pt"}). The MistNet model can be downloaded at
#' \url{https://s3.amazonaws.com/mistnet/mistnet_nexrad.pt}.
#'
#' @references
#' Dokter et al. (2011) is the main reference for the profiling algorithm
#' (vol2bird) underlying this function. When using the \code{mistnet} option,
#' please also cite Lin et al. 2019. When de-aliasing data, please also cite Haase et al. 2004.
#'
#' \itemize{
#'   \item Adriaan M. Dokter, Felix Liechti,
#'   Herbert Stark, Laurent Delobbe, Pierre Tabary, Iwan Holleman, 2011.
#'   Bird migration flight altitudes studied by a network of
#'   operational weather radars,
#'   Journal of the Royal Society Interface 8 (54), pp. 30--43.
#'   \url{https://doi.org/10.1098/rsif.2010.0116}
#'   \item Haase, G. and Landelius, T., 2004. Dealiasing of Doppler radar
#'   velocities using a torus mapping. Journal of Atmospheric and Oceanic
#'   Technology, 21(10), pp.1566--1573.
#'   \url{https://doi.org/10.1175/1520-0426(2004)021<1566:DODRVU>2.0.CO;2}
#'   \item Tsung-Yu Lin, Kevin Winner, Garrett Bernstein, Abhay Mittal, Adriaan M. Dokter
#'   Kyle G. Horton, Cecilia Nilsson, Benjamin M. Van Doren, Andrew Farnsworth
#'   Frank A. La Sorte, Subhransu Maji, Daniel Sheldon, 2019.
#'   MistNet: Measuring historical bird migration in the US
#'   using archived weather radar data and convolutional neural networks
#'   Methods in Ecology and Evolution 10 (11), pp. 1908--22.
#'   \url{https://doi.org/10.1111/2041-210X.13280}
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
#' # print some summary info:
#' profile
#' # convert profile to a data.frame:
#' as.data.frame(profile)
#' }
#'
#' # clean up:
#' file.remove("~/volume.h5")
calculate_vp <- function(file, vpfile = "", pvolfile_out = "",
                         autoconf = FALSE, verbose = FALSE,
                         mount = dirname(file[1]), sd_vvp_threshold,
                         rcs = 11, dual_pol = TRUE, rho_hv = 0.95, elev_min = 0,
                         elev_max = 90, azim_min = 0, azim_max = 360,
                         range_min = 5000, range_max = 35000, n_layer = 20L,
                         h_layer = 200, dealias = TRUE,
                         nyquist_min = if (dealias) 5 else 25,
                         dbz_quantity = "DBZH", mistnet = FALSE,
                         mistnet_elevations = c(0.5, 1.5, 2.5, 3.5, 4.5),
                         local_install, local_mistnet, pvolfile) {

  # check for deprecated input argument pvolfile
  calls <- names(sapply(match.call(), deparse))[-1]
  if (any("pvolfile" %in% calls)) {
    warning("argument 'pvolfile' is deprecated, please use 'file'")
  }

  # check input arguments
  assert_that(is.character(file), msg = "argument file is not a path to a file (or a vector of paths to files)")
  for (filename in file) {
    assert_that(file.exists(filename))
  }

  if(!are_equal(vpfile,"")){
    assert_that(is.writeable(dirname(vpfile)))
  }

  if(!are_equal(pvolfile_out,"")){
    assert_that(is.writeable(dirname(pvolfile_out)))
  }

  if (!is.logical(mistnet)) {
    stop("invalid 'mistnet' argument, should be logical")
  }
  if (mistnet && !.pkgenv$mistnet) {
    stop("MistNet has not been installed, see update_docker() for install instructions")
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

  if((missing(local_install) && !missing(local_mistnet)) || (!missing(local_install) && missing(local_mistnet))){
    stop("to use local vol2bird and mistnet model, specify both local_install and local_mistnet")
  }
  assert_that(is.numeric(mistnet_elevations))
  assert_that(length(mistnet_elevations) == 5)

  if (!.pkgenv$docker && missing(local_install)) {
    stop(
      "Requires a running Docker daemon.\nTo enable calculate_vp, start ",
      "your local Docker daemon, and run 'check_docker()' in R\n"
    )
  }

  assert_that(is.flag(verbose))

  assert_that(is.writeable(mount))

  if (!missing(sd_vvp_threshold)) {
    assert_that(is.number(sd_vvp_threshold))
    assert_that(sd_vvp_threshold >= 0)
  }

  assert_that(is.number(rcs))
  assert_that(rcs>0)

  assert_that(is.flag(dual_pol))

  assert_that(is.number(rho_hv))
  assert_that(rho_hv >= 0 & rho_hv <= 1, msg = "rho_hv should be a number between 0 and 1")

  assert_that(is.number(elev_min))
  assert_that(elev_min >= -90 & elev_min <= 90, msg = "elev_min is not a number between -90 and 90")

  assert_that(is.number(elev_max))
  assert_that(elev_max >= -90 & elev_max <= 90, msg = "elev_max is not a number between -90 and 90")

  assert_that(elev_max > elev_min, msg = "elev_max is not larger than elev_min")

  assert_that(is.number(azim_min))
  assert_that(azim_min >= 0 & azim_min <= 360, msg = "azim_min is not a number between 0 and 360")

  assert_that(is.number(azim_max))
  assert_that(azim_max >= 0 & azim_max <= 360, msg = "azim_max is not a number between 0 and 360")

  assert_that(is.number(range_min))
  assert_that(range_min > 0, msg = "range_min is not a positive number")

  assert_that(is.number(range_max))
  assert_that(range_max > 0, msg = "range_max is not a positive number")

  assert_that(range_max > range_min, msg = "range_max is not larger than range_min")

  assert_that(is.count(n_layer))

  assert_that(is.number(h_layer))
  assert_that(h_layer > 0, msg = "h_layer is not a positive number")

  assert_that(is.number(nyquist_min))
  assert_that(nyquist_min > 0, msg = "nyquist_min is not a positive number")

  assert_that(dbz_quantity %in% c("DBZ", "DBZH", "DBZV", "TH", "TV"), msg = "dbz_quantity is not one of DBZ, DBZH, DBZV, TH, TV")

  assert_that(is.flag(mistnet))

  assert_that(!(mistnet && !.pkgenv$mistnet), msg = "MistNet installation not found, see update_docker() for install instructions")

  assert_that(is.flag(dealias))

  assert_that(.pkgenv$docker | !missing(local_install),
      msg = paste("Requires a running Docker daemon.\nTo enable calculate_vp, start",
      "your local Docker daemon, and run 'check_docker()' in R\n"))

  filedir <- dirname(normalizePath(file[1], winslash = "/"))
  assert_that(is.writeable(filedir))

  assert_that(grepl(normalizePath(mount, winslash = "/"), filedir, fixed = TRUE),
      msg = paste("mountpoint 'mount' has to be a parent directory",
      "of input file 'file'"))

  # check whether vol2bird container supports multiple input files
  multi_file_support <- !is.null(.pkgenv$vol2bird_version) && !is.na(.pkgenv$vol2bird_version) && .pkgenv$vol2bird_version > numeric_version("0.3.20")
  if (!missing(local_install)) multi_file_support <- TRUE
  assert_that(!(length(file) > 1 && !multi_file_support),
              msg = paste("Current vol2bird installation does not support multiple input files.",
              "Provide a single input file containing a polar volume, or run update_docker() to update"))

  profile.tmp <- tempfile(tmpdir = filedir)

  if (missing(local_install)) {
    assert_that(mount_docker_container(normalizePath(mount, winslash = "/")) == 0,
                msg = "failed to start vol2bird Docker container, see check_docker()")
  }

  # put options file in place, to be read by vol2bird container
  opt.values <- c(
    as.character(c(
      rcs, rho_hv, elev_min, elev_max,
      azim_min, azim_max, range_min, range_max,
      n_layer, h_layer, nyquist_min, dbz_quantity
    )),
    if (dual_pol) "TRUE" else "FALSE",
    if (dealias) "TRUE" else "FALSE"
  )

  opt.names <- c(
    "SIGMA_BIRD", "RHOHVMIN", "ELEVMIN", "ELEVMAX",
    "AZIMMIN", "AZIMMAX", "RANGEMIN", "RANGEMAX", "NLAYER",
    "HLAYER", "MIN_NYQUIST_VELOCITY", "DBZTYPE", "DUALPOL",
    "DEALIAS_VRAD"
  )

  if (!missing(sd_vvp_threshold)) {
    opt.values <- c(as.character(sd_vvp_threshold), opt.values)
    opt.names <- c("STDEV_BIRD", opt.names)
  }

  if (mistnet) {
    opt.values <- c(opt.values, "TRUE",
                    paste("{", paste(as.character(mistnet_elevations), collapse = ", "), paste = "}", sep = ""),
                    ifelse(missing(local_install), "/MistNet/mistnet_nexrad.pt", normalizePath(local_mistnet)))
    opt.names <- c(opt.names, "USE_MISTNET", "MISTNET_ELEVS", "MISTNET_PATH")
  }

  opt <- data.frame(
    "option" = opt.names, "is" = rep("=", length(opt.values)),
    "value" = opt.values
  )
  if (missing(local_install)) {
    optfile <- paste(normalizePath(mount, winslash = "/"),
      "/options.conf",
      sep = ""
    )
  }
  else {
    optfile <- paste(getwd(), "/options.conf", sep = "")
  }

  if (file.exists(optfile)) {
    optfile_save <- paste(optfile, ".", format(Sys.time(), "%Y%m%d%H%M%S"), sep = "")
    warning(paste("options.conf file found in directory ", mount,
      ". Renamed to ", basename(optfile_save), " to prevent overwrite...",
      sep = ""
    ))
    file.rename(optfile, optfile_save)
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


  # we have a valid vol2bird version > 0.3.20, so we can use multiple file inputs
  if (multi_file_support) {
    pvolfile_docker <- paste("-i ", prefix, basename(file), sep = "", collapse = " ")
    profile.tmp.docker <- paste("-o ", prefix, basename(profile.tmp), sep = "")
    if (pvolfile_out != "") {
      pvolfile_out_docker <- paste("-p ", prefix, basename(pvolfile_out), sep = "")
    } else {
      pvolfile_out_docker <- ""
    }
  }
  else { # only single polar volume file input supported
    pvolfile_docker <- paste(prefix, basename(file), sep = "")
    profile.tmp.docker <- paste(prefix, basename(profile.tmp), sep = "")
    if (pvolfile_out != "") {
      pvolfile_out_docker <- paste(prefix, basename(pvolfile_out), sep = "")
    } else {
      pvolfile_out_docker <- ""
    }
  }

  docker_command <- paste(
    "docker exec vol2bird bash -c \"cd data && vol2bird ",
    pvolfile_docker, profile.tmp.docker,
    pvolfile_out_docker, "\""
  )

  # run vol2bird container
  if (.Platform$OS.type == "unix") {
    # on mac and linux:
    if (missing(local_install)) {
      result <- system(docker_command,
      ignore.stdout = !verbose
      )
    }
    else {
      # using a local install of vol2bird:
      result <- system(paste("bash -l -c \"", local_install, file, profile.tmp, pvolfile_out, "\""),
                       ignore.stdout = !verbose)
    }
  } else {
    # on Windows platforms:
    result <- suppressWarnings(system(docker_command))
  }
  if (result != 0) {
    if (file.exists(optfile)) file.remove(optfile)
    stop("failed to run vol2bird")
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
