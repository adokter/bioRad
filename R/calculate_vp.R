#' Calculate a vertical profile (`vp`) from a polar volume (`pvol`) file
#'
#' Calculates a vertical profile of biological scatterers (`vp`) from a polar
#' volume (`pvol`) file using the algorithm
#' [vol2bird](https://github.com/adokter/vol2bird/) (Dokter et al.
#' 2011 \doi{10.1098/rsif.2010.0116}).
#'
#' @param file Character (vector). Either a path to a single radar polar volume
#'   (`pvol`) file containing multiple scans/sweeps, or multiple paths to scan
#'   files containing a single scan/sweep. Or a single `pvol` object. The file data format should be either 1)
#'   [ODIM](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf)
#'    format, which is the implementation of the OPERA data information model in
#'   the [HDF5](https://support.hdfgroup.org/HDF5/) format, 2) a format
#'   supported by the [RSL
#'   library](https://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/) or 3) Vaisala
#'   IRIS (IRIS RAW) format.
#' @param vpfile Character. File name. When provided, writes a vertical profile
#'   file (`vpfile`) in the ODIM HDF5 format to disk.
#' @param pvolfile_out Character. File name. When provided, writes a polar
#'   volume (`pvol`) file in the ODIM HDF5 format to disk. Useful for converting
#'   RSL formats to ODIM.
#' @param autoconf Logical. When `TRUE`, default optimal configuration settings
#'   are selected automatically and other user settings are ignored.
#' @param verbose Logical. When `TRUE`, vol2bird `stdout` is piped to the R
#'   console.
#' @param warnings Logical. When `TRUE`, vol2bird warnings are piped to the R
#'   console.
#' @param mount Character. Directory path of the mount point for the Docker
#'   container (deprecated).
#' @param sd_vvp_threshold Numeric. Lower threshold for the radial velocity
#'   standard deviation (profile quantity `sd_vvp`) in m/s. Biological signals
#'   with `sd_vvp < sd_vvp_threshold` are set to zero. Defaults to 2 m/s for
#'   C-band radars and 1 m/s for S-band radars.
#' @param rcs Numeric. Radar cross section per bird to use, in cm^2.
#' @param dual_pol Logical. When `TRUE`, uses dual-pol mode, in which
#'   meteorological echoes are filtered using the correlation coefficient
#'   threshold `rho_hv`.
#' @param rho_hv Numeric. Lower threshold in correlation coefficient to use for
#'   filtering meteorological scattering.
#' @param single_pol Logical. When `TRUE`, uses precipitation filtering in single
#' polarization mode based on reflectivity and radial velocity quantities.
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
#'   `your/vol2bird_install_directory/vol2bird/bin/vol2bird.sh`). (deprecated)
#' @param local_mistnet Character. Path to local MistNet segmentation model in
#'   PyTorch format (e.g. `/your/path/mistnet_nexrad.pt`).
#'
#' @return A vertical profile object of class `vp`. When defined, output files
#'   `vpfile` and `pvolfile_out` are saved to disk.
#'
#' @export
#'
#' @details
#' ## Typical use
#'
#' Common arguments set by users are `file`, `vpfile` and `autoconf`.
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
#' You may point parameter `local_mistnet` to a local download of the MistNet segmentation model in
#' PyTorch format, e.g. `/your/path/mistnet_nexrad.pt`. The MistNet model can
#' be downloaded at <https://s3.amazonaws.com/mistnet/mistnet_nexrad.pt>.
#'
#' @seealso
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
#' * Haase G & Landelius T (2004)
#' Dealiasing of Doppler radar velocities using a torus mapping. Journal of
#' Atmospheric and Oceanic Technology 21(10), pp. 1566-1573.
#' \doi{10.1175/1520-0426(2004)021<1566:DODRVU>2.0.CO;2}
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
                         mount, sd_vvp_threshold,
                         rcs = 11, dual_pol = TRUE, rho_hv = 0.95, single_pol = TRUE,
                         elev_min = 0, elev_max = 90, azim_min = 0, azim_max = 360,
                         range_min = 5000, range_max = 35000, n_layer = 20,
                         h_layer = 200, dealias = TRUE,
                         nyquist_min = if (dealias) 5 else 25,
                         dbz_quantity = "DBZH", mistnet = FALSE,
                         mistnet_elevations = c(0.5, 1.5, 2.5, 3.5, 4.5),
                         local_install, local_mistnet) {
  if (inherits(file, "pvol")) {
    tmp_pvol_file <- tempfile(fileext = ".h5")
    write_pvolfile(file, file = tmp_pvol_file)
    withCallingHandlers(res <- calculate_vp(tmp_pvol_file,
      vpfile = vpfile, pvolfile_out = pvolfile_out,
      autoconf = autoconf, verbose = verbose, warnings = warnings,
      mount = mount, sd_vvp_threshold = sd_vvp_threshold,
      rcs = rcs, dual_pol = dual_pol, rho_hv = rho_hv, single_pol=single_pol,
      elev_min = elev_min, elev_max = 90, azim_min = 0, azim_max = 360,
      range_min = range_min, range_max = range_max, n_layer = n_layer,
      h_layer = h_layer, dealias = dealias,
      nyquist_min = nyquist_min,
      dbz_quantity = dbz_quantity, mistnet = mistnet,
      mistnet_elevations = mistnet_elevations,
      local_install = local_install, local_mistnet = local_mistnet
    ), error = function(e) {
      file.remove(tmp_pvol_file)
      e
    })
    file.remove(tmp_pvol_file)
    return(res)
  }
  rlang::check_installed('vol2birdR','to run `calculate_vp`')
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
  if (mistnet){
    if(missing(local_mistnet)){
      if (!vol2birdR::mistnet_exists()) {
        stop("MistNet has not been installed, see vol2birdR package documentation for install instructions.")
      }
    }
    else{
      if(!file.exists(local_mistnet)){
        stop(paste0("'",local_mistnet,"' does not exist, `local_mistnet` should specify the path of MistNet segmentation model"))
      }
    }
  }
  if (!is.logical(dealias)) {
    stop("`dealias` must be a logical value.")
  }
  if(!missing(mount))  {
    warning("mount argument is deprecated")
  }
  if(!missing(local_install)){
    warning("local_install argument is deprecated")
  }

  assert_that(is.numeric(mistnet_elevations))
  assert_that(length(mistnet_elevations) == 5)
  assert_that(is.flag(autoconf))
  assert_that(is.flag(verbose))
  assert_that(is.flag(warnings))
  if (!missing(sd_vvp_threshold)) {
    assert_that(is.number(sd_vvp_threshold))
    assert_that(sd_vvp_threshold >= 0)
  }
  assert_that(is.number(rcs))
  assert_that(rcs > 0)
  assert_that(is.flag(dual_pol))
  assert_that(is.number(rho_hv))
  assert_that(is.flag(single_pol))
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
    !(mistnet && !vol2birdR::mistnet_exists() && missing(local_mistnet)),
    msg = "Can't find MistNet installation, see vol2birdR package for install instructions.")
  assert_that(is.flag(dealias))

  filedir <- dirname(normalizePath(file[1], winslash = "/"))
  assert_that(is.writeable(filedir))

  profile.tmp <- tempfile()

  config <- vol2birdR::vol2bird_config()
  if(!autoconf){
    config$birdRadarCrossSection <- rcs
    config$rhohvThresMin <- rho_hv
    config$elevMin <- elev_min
    config$elevMax <- elev_max
    config$azimMin <- azim_min
    config$azimMax <- azim_max
    config$rangeMin <- range_min
    config$rangeMax <- range_max
    config$nLayers <- n_layer
    config$layerThickness <- h_layer
    config$minNyquist <- nyquist_min
    config$dbzType <- dbz_quantity
    config$dualPol <- dual_pol
    config$dealiasVrad <- dealias
    if (!missing(sd_vvp_threshold)) config$stdDevMinBird <- sd_vvp_threshold
  } else{
    # setting stdDevMinBird triggers it to be set according to wavelength (1 m/s for S-band, 2 m/s for C-band)
    config$stdDevMinBird <- -1
  }
  config$mistNetElevs <- mistnet_elevations
  config$useMistNet <- mistnet
  if(!missing(local_mistnet) & mistnet) config$mistNetPath <- local_mistnet

  # run vol2bird
  vol2birdR::vol2bird(file=file, config=config, vpfile=profile.tmp, pvolfile_out=pvolfile_out, verbose = verbose)

  # read output into a vp object
  output <- read_vpfiles(profile.tmp)

  # read output and clean up
  if (vpfile != "") {
    file.copy(profile.tmp, vpfile)
  }
  file.remove(profile.tmp)

  output
}
