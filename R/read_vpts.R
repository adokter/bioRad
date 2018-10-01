#' Read a time series of vertical profiles (\code{vpts}) from file
#'
#' @param file A text file containing the standard output (stdout) generated
#' by vol2bird (or the package function \code{calculate_vp}).
#' @param radar A string containing a radar identifier.
#' @param wavelength Radar wavelength in cm, or one of 'C' or 'S' for C-band
#' and S-band radar, respectively.
#'
#' @return An object inhereting from class \code{vpts}, see
#' \code{\link[=summary.vpts]{vpts}} for details.
#'
#' @export
#'
#' @examples
#' # locate example file:
#' vptszipfile <- system.file("extdata", "vpts.txt.zip", package = "bioRad")
#' \dontrun{
#' unzip(vptszipfile,"your/directory/and/file/name.txt")
#' # load time series:
#' ts <- read_vpts("your/directory/and/file/name.txt", radar = "KBGM", wavelength = "S")
#' ts
#' }
read_vpts <- function(file, radar, wavelength = "C") {
  # input checks
  if (!file.exists(file)) {
    stop(paste("File", file, "doesn't exist."))
  }
  if (file.size(file) == 0) {
    stop(paste("File", file, "is empty."))
  }
  if (missing(radar)) {
    stop("'radar' argument missing. Required to specify a radar identifier.")
  }
  if (missing(wavelength)) {
    warning(paste("No 'wavelength' argument provided, assuming radar operates",
      "at ", wavelength, "-band",
      sep = ""
    ))
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
  header.names.short <- c(
    "Date", "Time", "HGHT", "u", "v", "w", "ff", "dd",
    "sd_vvp", "gap", "dbz", "eta", "dens", "DBZH", "n",
    "n_dbz", "n_all", "n_dbz_all"
  )
  header.names.long <- c(
    "Date", "Time", "HGHT", "u", "v", "w", "ff", "dd",
    "sd_vvp", "head_bl", "head_ff", "head_dd", "head_sd",
    "gap", "dbz", "eta", "dens", "DBZH", "n", "n_dbz",
    "n_all", "n_dbz_all"
  )
  # read the data
  data <- read.table(file = file, header = FALSE)
  if (ncol(data) == 22) {
    colnames(data) <- header.names.long
  } else {
    colnames(data) <- header.names.short
  }
  # convert Time into a POSIXct date-time
  data$datetime <- as.POSIXct(paste(data$Date, sprintf("%04d", data$Time),
    sep = ""
  ),
  format = "%Y%m%d%H%M",
  tz = "UTC"
  )
  data$Date <- NULL
  data$Time <- NULL
  # sort
  data <- data[with(data, order(datetime, HGHT)), ]
  # remove duplicates
  data <- unique(data)
  # split into profiles
  data <- split(data, data$datetime)
  names(data) <- NULL
  # verify that profiles can be flattened
  datadim <- sapply(1:length(data), function(x) dim(data[[x]]))

  if (length(unique(datadim[1, ])) > 1) {
    mostFrequent <- sort(table(datadim[1, ]), decreasing = TRUE)[1]
    if (mostFrequent <= 1) {
      stop("Profiles are of unequal altitudinal dimensions, unable to merge")
    }
    mostFrequentNBins <- as.integer(names(mostFrequent))
    warning(paste(
      "Profiles are of unequal altitudinal dimensions or",
      "contain duplicates. Discarding", length(data) - mostFrequent,
      "of", length(data), "profiles, restricting to",
      mostFrequentNBins, "altitude bins."
    ))
    data <- data[datadim[1, ] == mostFrequentNBins]
  }
  # strip the datetime field
  dates <- .POSIXct(sapply(
    1:length(data),
    function(x) {
      data[[x]]$datetime[1]
    }
  ),
  tz = "UTC"
  )
  data <- lapply(
    data,
    function(x) {
      x["datetime"] <- NULL
      x
    }
  )
  # check whether the time series is regular
  difftimes <- difftime(dates[-1], dates[-length(dates)], units = "secs")
  if (length(unique(difftimes)) == 1) {
    regular <- TRUE
  } else {
    regular <- FALSE
  }
  # flatten the profiles
  profile.quantities <- names(data[[1]])
  vpsFlat <- lapply(
    profile.quantities,
    function(quantity) {
      sapply(data, "[[", quantity)
    }
  )
  names(vpsFlat) <- profile.quantities
  vpsFlat$HGHT <- NULL
  # prepare output
  heights <- data[[1]]$"HGHT"
  interval <- unique(heights[-1] - heights[-length(heights)])

  attributes <- list(
    where = data.frame(
      interval = interval,
      levels = length(heights)
    ),
    how = data.frame(wavelength = wavelength)
  )
  output <- list(
    radar = radar, dates = dates, heights = heights,
    daterange = .POSIXct(c(min(dates), max(dates)), tz = "UTC"),
    timesteps = difftimes, data = vpsFlat,
    attributes = attributes, regular = regular
  )
  class(output) <- "vpts"
  output
}
