#' Read a time series of vertical profiles (`vpts`) from file
#'
#' @param file A text file containing the standard output (stdout) generated
#' by vol2bird (or the package function `calculate_vp`).
#' @param lon numeric. Longitude of the radar in decimal degrees.
#' @param lat numeric. Latitude of the radar in decimal degrees.
#' @param height numeric. Height above sea level of the radar antenna in meters.
#' @param radar A string containing a radar identifier.
#' @param wavelength Radar wavelength in cm, or one of 'C' or 'S' for C-band
#' and S-band radar, respectively, in which case C-band wavelength is assumed
#' to be 5.3 cm and S-band wavelength 10.6 cm
#' @param sep the field separator character, see [read.table][utils::read.table]
#'
#' @return An object inheriting from class `vpts`, see
#' [`vpts()`][summary.vpts] for details.
#'
#' @export
#'
#' @examples
#' # locate example file:
#' vptsfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")
#' # load time series:
#' ts <- read_vpts(vptsfile, radar = "KBGM", wavelength = "S")
#' ts
read_vpts <- function(file, radar, lat, lon, height, wavelength = "C", sep="") {
  # input checks
  if (!file.exists(file)) {
    stop(paste("File", file, "doesn't exist."))
  }
  if (file.size(file) == 0) {
    stop(paste("File", file, "is empty."))
  }
  if (!missing(lat)) {
    if (!is.numeric(lat) || lat < -90 || lat > 90) {
      stop("'lat' should be numeric between -90 and 90 degrees")
    }
  }
  if (!missing(lon)) {
    if (!is.numeric(lon) || lat < -360 || lat > 360) {
      stop("'lon' should be numeric between -360 and 360 degrees")
    }
  }
  if (!missing(height)) {
    if (!is.numeric(height) || height < 0) {
      stop("'height' should be a positive number of meters above sea level")
    }
  }
  if (missing(radar) && sep =="") {
    stop("'radar' argument missing. Required to specify a radar identifier.")
  }
  if (missing(wavelength)) {
    warning(paste("No 'wavelength' argument provided, assuming radar operates",
      " at ", wavelength, "-band",
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

  # currently only two delimitors supported
  # "" for legacy vol2bird output, "," for csv output
  assert_that(sep == "" || sep == ",")

  # header of the data file
  header.names.short <- c(
    "Date", "Time", "height", "u", "v", "w", "ff", "dd",
    "sd_vvp", "gap", "dbz", "eta", "dens", "DBZH", "n",
    "n_dbz", "n_all", "n_dbz_all"
  )
  header.names.long <- c(
    "Date", "Time", "height", "u", "v", "w", "ff", "dd",
    "sd_vvp", "head_bl", "head_ff", "head_dd", "head_sd",
    "gap", "dbz", "eta", "dens", "DBZH", "n", "n_dbz",
    "n_all", "n_dbz_all"
  )
  # read the data

  if(sep!=""){
    # for parsing new csv format
    data <- read.table(file = file, header = TRUE, sep = sep)
    radar <- unique(data$radar)
    if(length(radar)>1){
      stop("file contains data for multiple radars")
    }
    data$radar <- NULL
    data$datetime <- parse_datetime(data$datetime)
    data$gap <- as.logical(data$gap)
  } else{
    # for parsing legacy vol2bird text output
    data <- read.table(file = file, header = FALSE, sep = sep)
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
  }

  # add profile_index to identify consecutive profiles
  data$new_profile_starts <- c(T, (data$height[-1] - data$height[-length(data$height)]) < 0)
  data$profile_index <- NA
  profile_index <- NULL # define profile_index to suppress devtools::check warning in next line
  data[which(data$new_profile_starts), "profile_index"] <- 1:length(which(data$new_profile_starts))
  data <- tidyr::fill(data, profile_index)

  data$new_profile_starts <- NULL
  data$Date <- NULL
  data$Time <- NULL

  # sort
  data <- data[with(data, order(datetime, profile_index, height)), ]

  # split into profiles
  data <- split(data, data$profile_index)
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
  datetime <- .POSIXct(sapply(
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
      x["profile_index"] <- NULL
      x
    }
  )

  # sort again, since split() changes ordering
  data <- data[order(datetime)]
  datetime <- sort(datetime)

  # check whether the time series is regular
  difftimes <- difftime(datetime[-1], datetime[-length(datetime)], units = "secs")
  difftimes
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
  vpsFlat$height <- NULL
  vpsFlat$profile_index <- NULL
  # prepare output
  heights <- data[[1]]$"height"
  interval <- unique(heights[-1] - heights[-length(heights)])

  attributes <- list(
    where = data.frame(
      interval = interval,
      levels = length(heights)
    ),
    how = data.frame(wavelength = wavelength)
  )
  if (!missing(height)) attributes$where$height <- height else attributes$where$height <- heights[1]
  if (!missing(lon)) attributes$where$lon <- lon
  if (!missing(lat)) attributes$where$lat <- lat

  output <- list(
    radar = radar, datetime = datetime, height = heights,
    daterange = .POSIXct(c(min(datetime), max(datetime)), tz = "UTC"),
    timesteps = difftimes, data = vpsFlat,
    attributes = attributes, regular = regular
  )
  class(output) <- "vpts"

  # remove duplicate profiles
  duplicate_timestamps <- which(output$timesteps == 0)
  if (length(duplicate_timestamps) > 0) {
    warning(paste("removed", length(duplicate_timestamps), "profiles with duplicate timestamps."))
    output <- output[-duplicate_timestamps]
  }

  output
}
