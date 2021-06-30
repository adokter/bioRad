#' Calculate a vertical profile (`vp`) from a polar volume (`pvol`) file
#'
#' Calculates a vertical profile of biological scatterers (`vp`) from a polar
#' volume (`pvol`) file using the algorithm
#' [vol2bird](https://github.com/adokter/vol2bird/) (Dokter et al.
#' 2011, \doi{10.1098/rsif.2010.0116}). Requires a running
#' [Docker](https://www.docker.com/) daemon, unless a local installation of
#' vol2bird is specified with `local_install`.
#'
#' @param file Character (vector). Either a path to a single radar polar volume
#'   (`pvol`) file containing multiple scans/sweeps, or multiple paths to scan
#'   files containing a single scan/sweep. Note that `pvol` objects are not
#'   supported. The file data format should be either 1)
#'   [ODIM](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf)
#'    format, which is the implementation of the OPERA data information model in
#'   the [HDF5](https://support.hdfgroup.org/HDF5/) format, 2) a format
#'   supported by the [RSL
#'   library](http://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/) or 3) Vaisala
#'   IRIS (IRIS RAW) format.
#' @param vpfile Character. File name. When provided, writes a vertical profile
#'   file (`vpfile`) in the ODIM HDF5 format to disk.
#' @param pvolfile_out Character. File name. When provided, writes a polar
#'   volume (`pvol`) file in the ODIM HDF5 format to disk. Useful for converting
#'   RSL formats to ODIM.
#' @param autoconf Logical. When `TRUE`, default optimal configuration settings
#'   are selected automatically and other user settings are ignored.
#' @param verbose Logical. When `TRUE`, Docker `stdout` is piped to the R
#'   console. Always `TRUE` on Windows.
#' @param warnings Logical. When `TRUE`, vol2bird warnings are piped to the R
#'   console.
#' @param mount Character. Directory path of the mount point for the Docker
#'   container.
#' @param sd_vvp_threshold Numeric. Lower threshold for the radial velocity
#'   standard deviation (profile quantity `sd_vvp`) in m/s. Biological signals
#'   with `sd_vvp < sd_vvp_threshold` are set to zero. Defaults to 2 m/s for
#'   C-band radars and 1 m/s for S-band radars.
#' @param rcs Numeric. Radar cross section per bird to use, in cm^2.
#' @param dual_pol Logical. When `TRUE`, uses dual-pol mode, in which
#'   meteorological echoes are filtered using the correlation coefficient
#'   `rho_hv`. When `FALSE`, uses single polarization mode based only on
#'   reflectivity and radial velocity quantities.
#' @param rho_hv Numeric. Lower threshold in correlation coefficient to use for
#'   filtering meteorological scattering.
#' @param elev_min Numeric. Minimum elevation angle to include, in degrees.
#' @param elev_max Numeric. Maximum elevation angle to include, in degrees.
#' @param azim_min Numeric. Minimum azimuth to include, in degrees clockwise
#'   from north.
#' @param azim_max Numeric. Maximum azimuth to include, in degrees clockwise
#'   from north.
#' @param range_min Numeric. Minimum range to include, in m.
#' @param range_max Numeric. Maximum range to include, in m.
#' @param n_layer Numeric. Number of altitude layers to use in generated
#'   profile.
#' @param h_layer Numeric. Width of altitude layers to use in generated profile,
#'   in m.
#' @param nyquist_min Numeric. Minimum Nyquist velocity of scans to include, in
#'   m/s.
#' @param dealias Logical. Whether to dealias radial velocities. This should
#'   typically be done when the scans in the polar volume have low Nyquist
#'   velocities (below 25 m/s).
#' @param dbz_quantity Name of the available reflectivity factor to use if not
#'   `DBZH` (e.g. `DBZV`, `TH`, `TV`).
#' @param mistnet Logical. Whether to use the MistNet segmentation model.
#' @param mistnet_elevations Numeric vector of length 5. Elevation angles to
#'   feed to the MistNet segmentation model, which expects exactly 5 elevation
#'   scans at 0.5, 1.5, 2.5, 3.5 and 4.5 degrees. Specifying different elevation
#'   angles may compromise segmentation results.
#' @param local_install Character. Path to local vol2bird installation (e.g.
#'   `your/vol2bird_install_directory/vol2bird/bin/vol2bird`) to use instead of
#'   the Docker container.
#' @param local_mistnet Character. Path to local MistNet segmentation model in
#'   PyTorch format (e.g. `/your/path/mistnet_nexrad.pt`) to use instead of the
#'   Docker container.
#'
#' @return A vertical profile object of class `vp`. When defined, output files
#'   `vpfile` and `pvolfile_out` are saved to disk.
#'
#' @export
#'
#' @details
#' ## Typical use
#'
#' Common arguments set by users are `file`, `vpfile`, `autoconf` and `mount`.
#' Turn on `autoconf` to automatically select the optimal parameters for a given
#' radar file. The default for C-band data is to apply rain-filtering in single
#' polarization mode and dual polarization mode when available. The default for
#' S-band data is to apply precipitation filtering in dual-polarization mode
#' only.
#'
#' Arguments that sometimes require non-default values are: `rcs`,
#' `sd_vvp_threshold`, `range_max`, `dual_pol`, `dealias`. Other arguments are
#' typically left at their defaults.
#'
#' ## mount
#'
#' On repeated calls of [calculate_vp()], the Docker container mount can be
#' recycled from one call to the next if subsequent calls share the same `mount`
#' argument. Re-mounting a Docker container takes time, therefore it is advised
#' to choose a mount point that is a parent directory of all volume files to be
#' processed, such that [calculate_vp()] calls are as fast as possible.
#'
#' ## sd_vvp_threshold
#'
#' For altitude layers with a VVP-retrieved radial velocity standard deviation
#' value below the threshold `sd_vvp_threshold`, the bird density `dens` is set
#' to zero (see vertical profile [`vp`][summary.vp()] class). This threshold
#' might be dependent on radar processing settings. Results from validation
#' campaigns so far indicate that 2 m/s is the best choice for this parameter
#' for most C-band weather radars, which is used as the C-band default. For
#' S-band, the default threshold is 1 m/s.
#'
#' ## rcs
#'
#' The default radar cross section (`rcs`) (11 cm^2) corresponds to the average
#' value found by Dokter et al. (2011) in a calibration campaign of a full
#' migration autumn season in western Europe at C-band. Its value may depend on
#' radar wavelength. `rcs` will scale approximately \eqn{M^{2/3}} with `M` the
#' bird's mass.
#'
#' ## dual_pol
#'
#' For S-band (radar wavelength ~ 10 cm), currently only `dual_pol = TRUE` mode
#' is recommended.
#'
#' ## azim_min / azim_max
#'
#' `azim_min` and `azim_max` only affects reflectivity-derived estimates in the
#' profile (`DBZH`, `eta`, `dens`), not radial-velocity derived estimates (`u`,
#' `v`, `w`, `ff`, `dd`, `sd_vvp`), which are estimated on all azimuths at all
#' times. `azim_min`, `azim_max` may be set to exclude an angular sector with
#' high ground clutter.
#'
#' ## range_min / range_max
#'
#' Using default values of `range_min` and `range_max` is recommended. Ranges
#' closer than 5 km tend to be contaminated by ground clutter, while range gates
#' beyond 35 km become too wide to resolve the default altitude layer width of
#' 200 meter (see [beam_width()]). `range_max` may be extended up to 40 km
#' (`40000`) for volumes with low elevations only, in order to extend coverage
#' to higher altitudes.
#'
#' ## h_layer
#'
#' The algorithm has been tested and developed for altitude layers with `h_layer
#' = 200`m. Smaller widths than 100 m are not recommended as they may cause
#' instabilities of the volume velocity profiling (VVP) and dealiasing routines,
#' and effectively lead to pseudo-replicated altitude data, since altitudinal
#' patterns smaller than the beam width cannot be resolved.
#'
#' ## dealias
#'
#' Dealiasing uses the torus mapping method by Haase et al. (2004).
#'
#' ## Local installation
#'
#' You can bypass the Docker container and speed up processing by installing
#' vol2bird locally (not on Windows). Point `local_install` to the path of your
#' local vol2bird executable, e.g.
#' `/your/vol2bird_install_directory/vol2bird/bin/vol2bird`. Your local vol2bird
#' executable will be called through a bash login shell. `LD_LIBRARY_PATH`
#' (Linux) or `DYLD_LIBRARY_PATH` (Mac) should be correctly specified in your
#' `.bashrc` or `.bash_profile` file and contain all the required shared
#' libraries by vol2bird. See vol2bird installation pages on
#' [GitHub](https://github.com/adokter/vol2bird) for details.
#'
#' When using MistNet with a local vol2bird installation, also point parameter
#' `local_mistnet` to your local download of the MistNet segmentation model in
#' PyTorch format, e.g. `/your/path/mistnet_nexrad.pt`. The MistNet model can
#' be downloaded at <https://s3.amazonaws.com/mistnet/mistnet_nexrad.pt>.
#'
#' @seealso
#' * [check_docker()]
#' * [summary.pvol()]
#' * [summary.vp()]
#'
#' @references
#' Dokter et al. (2011) is the main reference for the profiling algorithm
#' (vol2bird) underlying this function. When using the `mistnet` option, please
#' also cite Lin et al. (2019). When dealiasing data (`dealias`), please also
#' cite Haase et al. (2004).
#'
#' * Dokter AM, Liechti F, Stark H, Delobbe L,Tabary P, Holleman I (2011) Bird
#' migration flight altitudes studied by a network of operational weather
#' radars, Journal of the Royal Society Interface 8 (54), pp. 30-43.
#' \doi{10.1098/rsif.2010.0116}
#' * Haase G & Landelius T (2004) Dealiasing of Doppler radar velocities using a
#' torus mapping. Journal of Atmospheric and Oceanic Technology 21(10), pp.
#' 1566-1573. \doi{10.1175/1520-0426(2004)021<1566:DODRVU>2.0.CO;2}
#' * Lin T-Y, Winner K, Bernstein G, Mittal A, Dokter AM, Horton KG, Nilsson C,
#' Van Doren BM, Farnsworth A, La Sorte FA, Maji S, Sheldon D (2019) MistNet:
#' Measuring historical bird migration in the US using archived weather radar
#' data and convolutional neural networks. Methods in Ecology and Evolution 10
#' (11), pp. 1908-22. \doi{10.1111/2041-210X.13280}
#'
#' @examples
#' \dontrun{
#' # Locate and read the polar volume example file
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # Copy the file to a home directory with read/write permissions
#' file.copy(pvolfile, "~/volume.h5")
#'
#' # Calculate the profile
#' vp <- calculate_vp("~/volume.h5")
#'
#' # Get summary info
#' vp
#'
#' # Clean up
#' file.remove("~/volume.h5")
#' }
calculate_vp <- function(file, vpfile = "", pvolfile_out = "",
                         autoconf = FALSE, verbose = FALSE, warnings = TRUE,
                         mount = dirname(file[1]), sd_vvp_threshold,
                         rcs = 11, dual_pol = TRUE, rho_hv = 0.95, elev_min = 0,
                         elev_max = 90, azim_min = 0, azim_max = 360,
                         range_min = 5000, range_max = 35000, n_layer = 20,
                         h_layer = 200, dealias = TRUE,
                         nyquist_min = if (dealias) 5 else 25,
                         dbz_quantity = "DBZH", mistnet = FALSE,
                         mistnet_elevations = c(0.5, 1.5, 2.5, 3.5, 4.5),
                         local_install, local_mistnet) {

  # check input arguments
  assert_that(
    is.character(file),
    msg = "`file` must be a path to a file (or a vector of paths to files)."
  )
  for (filename in file) {
    assert_that(file.exists(filename))
  }
  if (!are_equal(vpfile, "")) {
    assert_that(is.writeable(dirname(vpfile)))
  }
  if (!are_equal(pvolfile_out, "")) {
    assert_that(is.writeable(dirname(pvolfile_out)))
  }
  if (!is.logical(mistnet)) {
    stop("`mistnet` must be a logical value.")
  }
  if (mistnet && !.pkgenv$mistnet) {
    stop("MistNet has not been installed, see update_docker() for install instructions.")
  }
  if (!is.logical(dealias)) {
    stop("`dealias` must be a logical value.")
  }
  if (file.access(mount, 0) == -1) {
    stop(glue("Can't find `mount` directory: {mount}"))
  }
  if (file.access(mount, 2) == -1) {
    stop(glue("No write permission to `mount` directory: {mount}"))
  }
  if ((missing(local_install) && !missing(local_mistnet)) || (!missing(local_install) && missing(local_mistnet))) {
    stop("To use local vol2bird and MistNet model, specify both `local_install` and `local_mistnet`.")
  }
  assert_that(is.numeric(mistnet_elevations))
  assert_that(length(mistnet_elevations) == 5)
  if (!.pkgenv$docker && missing(local_install)) {
    stop(
      "Requires a running Docker daemon.\nTo enable calculate_vp(), start ",
      "your local Docker daemon, and run check_docker() in R."
    )
  }
  assert_that(is.flag(autoconf))
  assert_that(is.flag(verbose))
  assert_that(is.flag(warnings))
  assert_that(is.writeable(mount))
  if (!missing(sd_vvp_threshold)) {
    assert_that(is.number(sd_vvp_threshold))
    assert_that(sd_vvp_threshold >= 0)
  }
  assert_that(is.number(rcs))
  assert_that(rcs > 0)
  assert_that(is.flag(dual_pol))
  assert_that(is.number(rho_hv))
  assert_that(
    rho_hv >= 0 & rho_hv <= 1,
    msg = "`rho_hv` must be a number between 0 and 1."
  )
  assert_that(is.number(elev_min))
  assert_that(
    elev_min >= -90 & elev_min <= 90,
    msg = "`elev_min` must be a number between -90 and 90."
  )
  assert_that(is.number(elev_max))
  assert_that(
    elev_max >= -90 & elev_max <= 90,
    msg = "`elev_max` must be a number between -90 and 90."
  )
  assert_that(
    elev_max > elev_min,
    msg = "`elev_max` must be larger than `elev_min`."
  )
  assert_that(is.number(azim_min))
  assert_that(
    azim_min >= 0 & azim_min <= 360,
    msg = "`azim_min` must be a number between 0 and 360."
  )
  assert_that(is.number(azim_max))
  assert_that(
    azim_max >= 0 & azim_max <= 360,
    msg = "`azim_max` must be a number between 0 and 360."
  )
  assert_that(is.number(range_min))
  assert_that(
    range_min >= 0,
    msg = "`range_min` must be a positive number."
  )
  assert_that(is.number(range_max))
  assert_that(
    range_max > 0,
    msg = "`range_max` must be a positive number."
  )
  assert_that(
    range_max > range_min,
    msg = "`range_max` must be larger than `range_min`."
  )
  assert_that(is.count(n_layer))
  assert_that(is.number(h_layer))
  assert_that(
    h_layer > 0,
    msg = "`h_layer` must be a positive number."
  )
  assert_that(is.number(nyquist_min))
  assert_that(
    nyquist_min > 0,
    msg = "`nyquist_min` must be a positive number."
  )
  assert_that(
    dbz_quantity %in% c("DBZ", "DBZH", "DBZV", "TH", "TV"),
    msg = "`dbz_quantity` must be either `DBZ`, `DBZH`, `DBZV`, `TH` or `TV`."
  )
  assert_that(is.flag(mistnet))
  assert_that(
    !(mistnet && !.pkgenv$mistnet),
    msg = "Can't find MistNet installation, see update_docker() for install instructions.")
  assert_that(is.flag(dealias))
  assert_that(
    .pkgenv$docker | !missing(local_install),
    msg = glue(
      "Requires a running Docker daemon.\nTo enable calculate_vp(), start ",
      "your local Docker daemon, and run check_docker() in R."
    )
  )
  filedir <- dirname(normalizePath(file[1], winslash = "/"))
  assert_that(is.writeable(filedir))
  assert_that(
    grepl(normalizePath(mount, winslash = "/"), filedir, fixed = TRUE),
    msg = "Mount point `mount` must be a parent directory of the input `file`."
  )

  # check whether vol2bird container supports multiple input files
  multi_file_support <- !is.null(.pkgenv$vol2bird_version) && !is.na(.pkgenv$vol2bird_version) && .pkgenv$vol2bird_version > numeric_version("0.3.20")
  if (!missing(local_install)) multi_file_support <- TRUE
  assert_that(!(length(file) > 1 && !multi_file_support),
    msg = glue(
      "Current vol2bird installation does not support multiple input files. ",
      "Provide a single input file containing a polar volume, or run ",
      "update_docker() to update."
    )
  )

  profile.tmp <- tempfile(tmpdir = filedir)

  if (missing(local_install)) {
    assert_that(
      mount_docker_container(normalizePath(mount, winslash = "/")) == 0,
      msg = "Failed to start vol2bird Docker container, see check_docker()."
    )
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
    opt.values <- c(
      opt.values, "TRUE",
      paste("{", paste(as.character(mistnet_elevations), collapse = ", "), paste = "}", sep = ""),
      ifelse(missing(local_install), "/MistNet/mistnet_nexrad.pt", normalizePath(local_mistnet))
    )
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
    warning(glue(
      "`options.conf` file found in directory {mount}. Renamed to ",
      "{basename(optfile_save)} to prevent overwrite."
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
        ignore.stdout = !verbose, ignore.stderr = !warnings
      )
    }
    else {
      # using a local install of vol2bird:
      result <- system(paste("bash -l -c \"", local_install, file, profile.tmp, pvolfile_out, "\""),
        ignore.stdout = !verbose, ignore.stderr = !warnings
      )
    }
  } else {
    # on Windows platforms:
    result <- suppressWarnings(system(docker_command))
  }
  if (result != 0) {
    if (file.exists(optfile)) file.remove(optfile)
    stop("Failed to run vol2bird.")
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
