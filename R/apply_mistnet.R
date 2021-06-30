#' Apply MistNet segmentation to a polar volume (`pvol`)
#'
#' Applies the MistNet segmentation model to a polar volume (`pvol`) file on
#' disk and loads the resulting segmentation as a polar volume (`pvol`) object.
#'
#' @param file Character. Path to a polar volume (`pvol`) file.
#' @param pvolfile_out Character. File name. When provided, writes a polar
#'   volume (`pvol`) file to disk that includes the Mistnet segmentation
#'   results.
#' @param load Logical. When `TRUE`, returns a `pvol` object.
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
#' @inheritParams calculate_vp
#'
#' @return When `load` is `TRUE`, a polar volume (`pvol`) object with the
#'   Mistnet segmentation results. When `load` is `FALSE`, `TRUE` on success.
#'
#' @export
#'
#' @details
#' MistNet (Lin et al. 2019) is a deep convolutional neural network that has
#' been trained using labels derived from S-band dual-polarization data across
#' the US NEXRAD network. Its purpose is to screen out areas of precipitation in
#' weather radar data, primarily legacy data for which dual-polarization data
#' are not available. Because the network has been trained on S-band data, it
#' may not perform as well on C-band.
#'
#' MistNet requires three single-polarization parameters as input: reflectivity
#' (`DBZH`), radial velocity (`VRADH`), and spectrum width (`WRADH`), at 5
#' specific elevation angles (0.5, 1.5, 2.5, 3.5 and 4.5 degrees). Based on
#' these data it can estimate a segmentation mask that identifies pixels with
#' weather that should be removed when interested in biological data only.
#'
#' MistNet will calculate three class probabilities (from 0 to 1, with 1
#' corresponding to a 100% probability) as additional scan parameters to the
#' polar volume:
#' * `BACKGROUND`: Class probability that no signal was detected above the noise
#' level of the radar.
#' * `WEATHER`: Class probability that weather was detected.
#' * `BIOLOGY`: Class probability that biological scatterers were detected.
#'
#' These class probabilities are only available for the 5 input elevations used
#' as input for the MistNet model. Based on all the class probabilities a final
#' weather segmentation map is calculated, stored as scan parameter `CELL`,
#' which is available for all elevation scans.
#' * `CELL`: Final weather segmentation, with values > 1 indicating pixels
#' classified as weather and values equal to 1 indicating pixels that are
#' located within 5 km distance of a weather pixels.
#'
#' A pixel is classified as weather if the class probability `WEATHER` > 0.45 or
#' when the average class probability for rain across all five MistNet elevation
#' scans at that spatial location > 0.45.
#'
#' MistNet may run more slowly on Windows than on Linux or Mac OS X.
#'
#' @seealso
#' * [check_docker()]
#' * [calculate_vp()]
#'
#' @references
#' Please cite this publication when using MistNet:
#' * Lin T-Y, Winner K, Bernstein G, Mittal A, Dokter AM, Horton KG, Nilsson C,
#' Van Doren BM, Farnsworth A, La Sorte FA, Maji S, Sheldon D (2019) MistNet:
#' Measuring historical bird migration in the US using archived weather radar
#' data and convolutional neural networks. Methods in Ecology and Evolution 10
#' (11), pp. 1908-22. \doi{10.1111/2041-210X.13280}
#'
#' @examples
#' \dontrun{
#' # Download a NEXRAD file and save as KBGM_example
#' download.file(
#'   "https://noaa-nexrad-level2.s3.amazonaws.com/2019/10/01/KBGM/KBGM20191001_000542_V06",
#'   "~/KBGM_example"
#' )
#'
#' # Calculate MistNet segmentation
#' mistnet_pvol <- apply_mistnet("~/KBGM_example")
#'
#' # Print summary info for the segmented elevation scan at the 0.5 degree,
#' # verify new parameters BIOLOGY, WEATHER, BACKGROUND and CELL have been added
#' scan <- get_scan(mistnet_pvol, 0.5)
#' scan
#'
#' # Project the scan as a ppi
#' ppi <- project_as_ppi(scan, range_max = 100000)
#'
#' # Plot the reflectivity parameter
#' plot(ppi, param = "DBZH")
#'
#' # Plot the MistNet class probability [0-1] for weather
#' plot(ppi, param = "WEATHER")
#'
#' # Plot the MistNet class probability [0-1] for biology
#' plot(ppi, param = "BIOLOGY")
#'
#' # Plot the final segmentation result, with values >1 indicating
#' # areas classified as weather, and value 1 pixels that fall within an
#' # additional 5 km fringe around weather areas
#' plot(ppi, param = "CELL")
#'
#' # Remove file
#' file.remove("~/KBGM_example")
#' }
apply_mistnet <- function(file, pvolfile_out, verbose = FALSE,
                          mount = dirname(file), load = TRUE,
                          mistnet_elevations = c(0.5, 1.5, 2.5, 3.5, 4.5),
                          local_install, local_mistnet) {
  tryCatch(
    apply_mistnet_body(file, pvolfile_out, verbose, mount, load,
                       mistnet_elevations, local_install, local_mistnet),
    error = function(err) {
      rhdf5::h5closeAll()
      stop(err)
    }
  )
}

apply_mistnet_body <- function(file, pvolfile_out, verbose = FALSE,
                          mount = dirname(file), load = TRUE,
                          mistnet_elevations = c(0.5, 1.5, 2.5, 3.5, 4.5),
                          local_install, local_mistnet) {

  assert_that(file.exists(file))

  if (!.pkgenv$mistnet) {
    stop("MistNet has not been installed, see update_docker() for install instructions")
  }

  assert_that(is.numeric(mistnet_elevations))
  assert_that(length(mistnet_elevations) == 5)

  if((missing(local_install) && !missing(local_mistnet)) || (!missing(local_install) && missing(local_mistnet))){
    stop("to use local vol2bird and mistnet model, specify both local_install and local_mistnet")
  }

  if (!missing(pvolfile_out)) {
    if (!file.exists(dirname(pvolfile_out))) {
      stop(paste("output directory", dirname(pvolfile_out), "not found"))
    }
    if (file.access(dirname(pvolfile_out), 2) == -1) {
      stop(paste("No write permission in directory", dirname(pvolfile_out)))
    }
  }

  opt.names <- c("USE_MISTNET", "MISTNET_ELEVS", "MISTNET_PATH")
  opt.values <- c(
    "TRUE",
    paste("{", paste(as.character(mistnet_elevations), collapse = ", "), paste = "}", sep = ""),
    ifelse(missing(local_install), "/MistNet/mistnet_nexrad.pt", normalizePath(local_mistnet))
  )

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

  # write options.conf file
  write.table(opt,
    file = optfile, col.names = FALSE,
    row.names = FALSE, quote = FALSE
  )

  # apply mistnet and generate pvol.
  pvol_tmp <- nexrad_to_odim_tempfile(file, verbose, mount, local_install)

  if (load) output <- read_pvolfile(pvol_tmp) else output <- TRUE

  # clean up pvol
  if (missing(pvolfile_out)) {
    file.remove(pvol_tmp)
  } else {
    file.rename(pvol_tmp, pvolfile_out)
  }
  # clean up options.conf
  if (file.exists(optfile)) {
    file.remove(optfile)
  }

  return(output)
}
