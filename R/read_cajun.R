#' Read a vertical profile (`vp`) from UMASS Cajun text file
#'
#' @param file A text file containing the standard output (stdout) generated
#' by UMASS Cajun pipeline
#' @param rcs numeric. Radar cross section per bird in cm^2.
#' @param wavelength Radar wavelength in cm, or one of 'C' or 'S' for C-band and S-band radar, respectively,
#' in which case C-band wavelength is assumed to be 5.3 cm and S-band wavelength 10.6 cm
#'
#' @return An object inheriting from class `vp`, see
#' [`vp()`][summary.vp] for details.
#'
#' @export
read_cajun <- function(file, rcs = 11, wavelength = "S") {
  # input checks
  if (!file.exists(file)) {
    stop(paste("File", file, "doesn't exist."))
  }
  if (file.size(file) == 0) {
    stop(paste("File", file, "is empty."))
  }
  if (wavelength == "C") {
    wavelength <- 5.3
  }
  if (wavelength == "S") {
    wavelength <- 10.6
  }
  if (!is.numeric(wavelength) || length(wavelength) > 1) {
    stop("Not a valid 'wavelength' argument.")
  }

  # header of the data file
  header.names.sorted <- c(
    "height", "u", "v", "w", "ff", "dd",
    "sd_vvp", "gap", "dbz", "eta", "dens", "DBZH", "n",
    "n_dbz", "n_all", "n_dbz_all",
    "elev1", "nvolumes_gr35_e1", "elev2", "nvolumes_gr35_e2", "vcp", "percent_rain"
  )
  header.names.cajun <- c("bin_lower", "height", "linear_eta", "nbins", "direction", "speed", "u", "v", "rmse", "elev1", "nvolumes_gr35_e1", "elev2", "nvolumes_gr35_e2", "vcp", "linear_eta_unfiltered", "percent_rain")
  header.names.biorad <- c("height", "height_mean", "eta", "n_dbz_all", "dd", "ff", "u", "v", "sd_vvp", "elev1", "nvolumes_gr35_e1", "elev2", "nvolumes_gr35_e2", "vcp", "linear_eta_unfiltered", "percent_rain")

  # read the data
  data <- read.table(file = file, header = TRUE, sep = ",")

  # rename columns to bioRad standard
  colnames(data) <- header.names.biorad

  # add missing quantities
  data$DBZH <- eta_to_dbz(data$linear_eta_unfiltered, wavelength = wavelength)
  data$w <- NA
  data$gap <- FALSE
  data$dbz <- eta_to_dbz(data$eta, wavelength = wavelength)
  data$dens <- data$eta / rcs
  data$n <- NA
  data$n_dbz <- data$n_dbz_all * (1 - data$percent_rain / 100)
  data$n_all <- NA

  # remove redundant quantities
  data$height_mean <- NULL

  # sort into bioRad order
  data <- data[, header.names.sorted]

  # extract info from filename
  datetime <- as.POSIXct(substr(basename(file), 5, 19), format = "%Y%m%d_%H%M%S", tz = "UTC")
  radar <- substr(basename(file), 1, 4)

  # prepare output
  height <- data$height
  interval <- unique(height[-1] - height[-length(height)])

  attributes <- list(
    where = data.frame(
      interval = interval,
      levels = length(height),
      height = 0 # cajun altitudes are relative to antenna level
    ),
    what = data.frame(source = basename(file), stringsAsFactors = F),
    how = data.frame(wavelength = wavelength, task = "UMASS Cajun")
  )
  output <- list(
    radar = radar, datetime = datetime, data = data,
    attributes = attributes
  )
  class(output) <- "vp"
  output
}
