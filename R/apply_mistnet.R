#' Apply MistNet segmentation to a polar volume
#'
#' Apply MistNet segmentation model to a polar volume file on disk, and load the
#' resultant segmentation as a polar volume (\code{pvol}) object.
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
#' @param local_install (optional) String with path to local vol2bird binary
#' (e.g. \code{"/your/vol2bird_install_directory/vol2bird/bin/vol2bird.sh"}),
#' to use local installation instead of Docker container
#' @param local_mistnet (optional) String with path to local mistnet segmentation model
#' in PyTorch format (e.g. \code{"/your/path/mistnet_nexrad.pt"}),
#' to use local installation instead of Docker container.
#'
#' @inheritParams calculate_vp
#'
#' @return If parameter \code{load} is TRUE an object of class \code{pvol} on success.
#' If parameter \code{load} is FALSE, \code{TRUE} on success.
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
#' \describe{
#' \item{"\code{BACKGROUND}"}{class probability that no signal was detected above the noise level of the radar}
#' \item{"\code{WEATHER}"}{class probability that weather was detected}
#' \item{"\code{BIOLOGY}"}{class probability that biological scatterers were detected}
#' }
#' These class probabilities are only available for the 5 input elevations used
#' as input for the MistNet model. Based on all the class probabilities a final weather segmentation map calculated,
#' stored as scan parameter \code{CELL}, which is available for all elevation scans.
#' \describe{
#' \item{"\code{CELL}"}{Final weather segmentation, with values > 1 indicating pixels
#' classified as weather, and values equal to 1 indicating pixels
#' that are located within 5 km distance of a weather pixels}
#' }
#'
#' A pixel is classified as weather if the class probability \code{WEATHER} > 0.45
#' or when the average class probability for rain across all five MistNet elevation
#' scans at that spatial location > 0.45.
#'
#' MistNet may run more slowly on Windows than on Linux or Mac OSX.
#'
#' See Lin et al. 2019 for details.
#'
#' @references
#' Please also cite this publication when using MistNet:
#' \itemize{
#'   \item Lin T-Y, Winner K, Bernstein G, Mittal A, Dokter AM, Horten KG,
#'   Nilsson C, Van Doren B, Farnsworth A, La Sorte FA, Maji S, Sheldon D (2019)
#'   MistNet: Measuring historical bird migration in the US using archived
#'   weather radar data and convolutional neural networks. Methods in Ecology
#'   and Evolution 10: 1908– 1922. \doi{10.1111/2041-210X.13280}
#' }
#' @examples
#' \dontrun{
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

  if (!vol2birdR::mistnet_exists() && missing(local_mistnet)) {
    stop("MistNet has not been installed, see vol2birdR package documentation for install instructions")
  }

  assert_that(is.numeric(mistnet_elevations))
  assert_that(length(mistnet_elevations) == 5)

  if(!missing(local_mistnet)){
    if(!file.exists(local_mistnet)){
      stop(paste0("'",local_mistnet,"' does not exist, `local_mistnet` should specify the path of MistNet segmentation model"))
    }
  }

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
