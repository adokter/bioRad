#' Apply MistNet segmentation to a polar volume
#'
#' Apply MistNet segmentation model to a polar volume file on disk, and load the
#' resultant segmentation as a polar volume (`pvol`) object.
#'
#' @param file character. File path for a radar polar volume.
#' @param pvolfile_out character. (optional) Filename for the polar volume to be
#' stored, including the MistNet segmentation results
#' @param load on completion load the data
#' @param mistnet_elevations numeric vector of length 5.
#' Elevation angles to feed to the MistNet
#' segmentation model, which expects exactly 5 elevation scans
#' at 0.5, 1.5, 2.5, 3.5 and 4.5 degrees. Specifying different
#' elevation angles may compromise segmentation results.
#' @param local_install (deprecated) String with path to local vol2bird binary
#' (e.g. `"/your/vol2bird_install_directory/vol2bird/bin/vol2bird.sh"`),
#' to use local installation instead of Docker container
#' @param local_mistnet (optional) String with path to local mistnet segmentation model
#' in PyTorch format (e.g. `"/your/path/mistnet_nexrad.pt"`)
#'
#' @inheritParams calculate_vp
#'
#' @return If parameter `load` is TRUE an object of class `pvol` on success.
#' If parameter `load` is FALSE, `TRUE` on success.
#'
#' @export
#'
#' @details
#' MistNet is a deep convolutional neural network that has been trained
#' using labels derived from S-band dual-polarization data across the US
#' NEXRAD network.
#'
#' It's purpose is to screen out areas of precipitation in weather radar data,
#' primarily legacy data for which dual-polarization data are not available.
#'
#' Because the network has been trained on S-band data, it may not
#' perform as well on C-band.
#'
#' MistNet requires three single-polarization parameters as input: reflectivity (DBZH),
#' radial velocity (VRADH), and spectrum width (WRADH), at 5 specific
#' elevation angles (0.5, 1.5, 2.5, 3.5 and 4.5 degrees). Based on these data
#' it can estimate a segmentation mask that identifies pixels with weather
#' that should be removed when interested only in biological data.
#'
#' MistNet will calculate three class probabilities (from 0 to 1, with 1 corresponding
#' to a 100% probability) as additional scan parameters to the polar volume:
#'
#' * `BACKGROUND`: Class probability that no signal was detected above the noise
#'   level of the radar
#' * `WEATHER`: Class probability that weather was detected
#' * `BIOLOGY`: Class probability that biological scatterers were detected
#'
#' These class probabilities are only available for the 5 input elevations used
#' as input for the MistNet model. Based on all the class probabilities a final weather segmentation map calculated,
#' stored as scan parameter `CELL`, which is available for all elevation scans.

#' * `CELL`: Final weather segmentation, with values > 1 indicating pixels
#'  classified as weather, and values equal to 1 indicating pixels that are
#'  located within 5 km distance of a weather pixels
#'
#' A pixel is classified as weather if the class probability `WEATHER` > 0.45
#' or when the average class probability for rain across all five MistNet elevation
#' scans at that spatial location > 0.45.
#'
#' MistNet may run more slowly on Windows than on Linux or Mac OSX.
#'
#' See Lin et al. 2019 for details.
#'
#' @references
#' Please also cite this publication when using MistNet:
#'
#' * Lin T-Y, Winner K, Bernstein G, Mittal A, Dokter AM, Horten KG,
#'   Nilsson C, Van Doren B, Farnsworth A, La Sorte FA, Maji S, Sheldon D (2019)
#'   MistNet: Measuring historical bird migration in the US using archived
#'   weather radar data and convolutional neural networks. Methods in Ecology
#'   and Evolution 10: 1908– 1922. \doi{10.1111/2041-210X.13280}
#'
#' @examples
#' \dontrun{
#' # make sure you have installed the MistNet libraries and model, using:
#' # vol2birdR::install_mistnet()
#' # vol2birdR::install_mistnet_model()
#'
#' # download a NEXRAD file, save as KBGM_example
#' download.file(paste("https://noaa-nexrad-level2.s3.amazonaws.com/",
#'   "2019/10/01/KBGM/KBGM20191001_000542_V06",
#'   sep = ""
#' ), "~/KBGM_example")
#'
#' # calculate MistNet segmentation:
#' mistnet_pvol <- apply_mistnet("~/KBGM_example")
#'
#' # print summary info for the segmented elevation scan at 0.5 degree,
#' # verify new parameters BIOLOGY, WEATHER, BACKGROUND and CELL have been added:
#' my_scan <- get_scan(mistnet_pvol, 0.5)
#'
#' # project the scan as a ppi:
#' my_ppi <- project_as_ppi(my_scan, range_max = 100000)
#'
#' # plot the reflectivity parameter:
#' plot(my_ppi, param = "DBZH")
#'
#' # plot the MistNet class probability [0-1] for weather
#' plot(my_ppi, param = "WEATHER")
#'
#' # plot the MistNet class probability [0-1] for biology
#' plot(my_ppi, param = "BIOLOGY")
#'
#' # plot the final segmentation result, with values >1 indicating
#' # areas classified as weather, and value 1 pixels that fall within an
#' # additional 5 km fringe around weather areas.
#' plot(my_ppi, param = "CELL")
#'
#' # clean up:
#' file.remove("~/KBGM_example")
#' }
apply_mistnet <- function(file, pvolfile_out, verbose = FALSE,
                          mount = dirname(file), load = TRUE,
                          mistnet_elevations = c(0.5, 1.5, 2.5, 3.5, 4.5),
                          local_install, local_mistnet) {
  tryCatch(apply_mistnet_body(file, pvolfile_out, verbose, mount, load, mistnet_elevations, local_install, local_mistnet),
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

  if (!vol2birdR::mistnet_exists()) {
    stop("MistNet has not been installed, see vol2birdR package documentation for install instructions")
  }

  assert_that(is.numeric(mistnet_elevations))
  assert_that(length(mistnet_elevations) == 5)

  if(!missing(local_mistnet)){
    if(!file.exists(local_mistnet)){
      stop(paste0("'",local_mistnet,"' does not exist, `local_mistnet` should specify the path of MistNet segmentation model"))
    }
  }

  if (!missing(pvolfile_out)) {
    if (!file.exists(dirname(pvolfile_out))) {
      stop(paste("output directory", dirname(pvolfile_out), "not found"))
    }
    if (file.access(dirname(pvolfile_out), 2) == -1) {
      stop(paste("No write permission in directory", dirname(pvolfile_out)))
    }
  }

  config <- vol2birdR::vol2bird_config()
  config$useMistNet <- TRUE
  config$mistNetElevs <- mistnet_elevations
  if(!missing(local_mistnet)) config$mistNetPath <- local_mistnet

  pvol_tmp <- tempfile()

  # apply mistnet and generate pvol.
  vol2birdR::rsl2odim(file=file, config=config, pvolfile_out=pvol_tmp, verbose=verbose)

  if (load) output <- read_pvolfile(pvol_tmp) else output <- TRUE

  # clean up pvol
  if (missing(pvolfile_out)) {
    file.remove(pvol_tmp)
  } else {
    file.rename(pvol_tmp, pvolfile_out)
  }

  return(output)
}
