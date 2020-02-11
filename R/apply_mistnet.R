#' Apply MistNet segmentation to a polar volume
#'
#' Apply MistNet segmentation model to a polar volume file on disk, and load the
#' resultant segmentation as a polar volume (pvol) object.
#' @param file character. File path for a radar polar volume.
#' @param pvolfile_out character. Filename for the polar volume to be
#' stored, including the MistNet segmentation results
#' @param load on completion load the data
#' @param mistnet_elevations numeric vector of length 5.
#' Elevation angles to feed to the MistNet
#' segmentation model, which expects exactly 5 elevation scans
#' at 0.5, 1.5, 3.5, 3.5 and 4.5 degrees. Specifying different
#' elevation angles may compromise segmentation results.
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
#' elevation angles (0.5, 1.5, 3.5, 3.5 and 4.5 degrees). Based on these data
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
#' scans at that spatial location > 0.45
#'
#' MistNet may run more slowly on Windows than on Linux or Mac OSX.
#'
#' See Lin et al. 2019 for details.
#'
#' @references
#' Please also cite this publication when using MistNet:
#' \itemize{
#'   \item Tsung‐Yu Lin, Kevin Winner, Garrett Bernstein, Abhay Mittal, Adriaan M. Dokter
#'   Kyle G. Horton, Cecilia Nilsson, Benjamin M. Van Doren, Andrew Farnsworth
#'   Frank A. La Sorte, Subhransu Maji, Daniel Sheldon, 2019.
#'   MistNet: Measuring historical bird migration in the US
#'   using archived weather radar data and convolutional neural networks.
#'   Methods in Ecology and Evolution 10 (11), pp. 1908--22.
#'   \url{https://doi.org/10.1111/2041-210X.13280}
#' }
#' @examples
#' # locate example polar volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # copy to a home directory with read/write permissions:
#' file.copy(pvolfile, "~/volume.h5")
#'
#' # calculate MistNet segmentation:
#' \dontrun{
#' example_pvol <- apply_mistnet("~/volume.h5")
#' # plot the MistNet class probability [0-1] for biology, for the first elevation scan:
#' plot(example_pvol$scans[[1]], param="BIOLOGY")
#' # plot the final segmentation result, with values >1 indicating
#' # areas classified as weather, and value 1 pixels that fall within an
#' # additional 5 km fringe around weather areas.
#' plot(example_pvol$scans[[1]], param="CELL")
#' }
#'
#' # clean up:
#' file.remove("~/volume.h5")
apply_mistnet <- function(file, pvolfile_out, verbose = FALSE,
                          mount = dirname(file), load=TRUE,
                          mistnet_elevations = c(0.5, 1.5, 2.5, 3.5, 4.5)){

  assert_that(file.exists(file))

  if(!.pkgenv$mistnet){
    stop("MistNet has not been installed, see update_docker() for install instructions")
  }

  assert_that(is.numeric(mistnet_elevations))
  assert_that(length(mistnet_elevations) == 5)

  if(!missing(pvolfile_out)){
    if (!file.exists(dirname(pvolfile_out))) {
      stop(paste("output directory", dirname(pvolfile_out), "not found"))
    }
    if (file.access(dirname(pvolfile_out), 2) == -1) {
      stop(paste("No write permission in directory", dirname(pvolfile_out)))
    }
  }

  opt.names = c("USE_MISTNET","MISTNET_ELEVS")
  opt.values = c("TRUE",
                 paste("{",paste(as.character(mistnet_elevations), collapse=", "),paste="}", sep=""))

  opt <- data.frame(
    "option" = opt.names, "is" = rep("=", length(opt.values)),
    "value" = opt.values
  )
  optfile <- paste(normalizePath(mount, winslash = "/"),
                   "/options.conf",
                   sep = ""
  )

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
  pvol_tmp <- nexrad_to_odim_tempfile(file, verbose, mount)

  if(load) output = read_pvolfile(pvol_tmp) else output = TRUE

  # clean up pvol
  if (missing(pvolfile_out)) {
    file.remove(pvol_tmp)
  } else{
    file.rename(pvol_tmp, pvolfile_out)
  }
  # clean up options.conf
  if (file.exists(optfile)) {
    file.remove(optfile)
  }

  return(output)
}
