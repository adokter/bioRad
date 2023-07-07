
#' Convert a dataframe into a vpts object
#'
#' @param data a dataframe created from a VPTS CSV file
#' @returns a bioRad vpts object
#' @examples
#' # locate example file in VPTS CSV format:
#' df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))
#' # convert the data.frame to a vpts object:
#' as.vpts(df)
#' @export
as.vpts <- function(data) {
  assertthat::assert_that(inherits(data,"data.frame"))

  height <- datetime <- source_file <- radar <- NULL

  # Throw error if nrows per height are not identical

  assertthat::assert_that(
    isFactor(dim(data)[1], length(unique(data$height))) > 0,
    msg = "Number of rows per height variable must be identical"
  )

  radar <- unique(data[["radar"]])

  # Check radar is unique
  assertthat::assert_that(
    length(radar) == 1,
    msg = "`data` must contain data of a single radar."
  )

  if (!exists("cached_schema")) {
    # Load the schema from the data directory and cache it
    cached_schema <- jsonlite::fromJSON(system.file("extdata", "vpts-csv-table-schema.json", package = "bioRad"),
      simplifyDataFrame = FALSE, simplifyVector = TRUE
    )
  }

  data <- dplyr::mutate(
    data,
    radar = as.factor(radar),
    datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )

  if("source_file" %in% colnames(data)){
    data <- dplyr::mutate(
        data,
        source_file = as.factor(source_file)
    )
  }

  # Check whether time series is regular
  heights <- as.integer(unique(data[["height"]]))

  # Subset timestamps by first sampled height
  datetime <- data[data[["height"]] == heights[1], ][["datetime"]]
  datetime <- as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")


  # Determine regularity
  difftimes <- difftime(datetime[-1], datetime[-length(datetime)], units = "secs")
  if (length(unique(difftimes)) == 1) {
    regular <- TRUE
  } else {
    regular <- FALSE
  }

  # Get attributes
  radar_height <- data[["radar_height"]][1]
  interval <- unique(heights[-1] - heights[-length(heights)])
  wavelength <- data[["radar_wavelength"]][1]
  if(length(unique(data[["radar_longitude"]]))>1) warning(paste0("multiple `radar_longitude` values found, storing only first (",lon,") as the functional attribute"))
  lon <- data[["radar_longitude"]][1]
  if(length(unique(data[["radar_latitude"]]))>1) warning(paste0("multiple `radar_latitude` values found, storing only first (",lat,") as the functional attribute"))
  lat <- data[["radar_latitude"]][1]
  if(length(unique(data[["rcs"]]))>1) warning(paste0("multiple `rcs` values found, storing only first (",rcs,") as the functional attribute"))
  rcs <- data[["rcs"]][1]
  if(length(unique(data[["sd_vvp_threshold"]]))>1) warning(paste0("multiple `sd_vvp_threshold` values found, storing only first (",sd_vvp_threshold,") as the functional attribute"))
  sd_vvp_threshold <- data[["sd_vvp_threshold"]][1]

  # Convert dataframe
  maskvars <- c("radar", "radar_latitude", "radar_longitude", "radar_height", "radar_wavelength", "source_file", "datetime", "height", "sunrise", "sunset", "day")

  data <- df_to_mat_list(data, maskvars, cached_schema)

  # Create vpts object
  output <- list(
    radar = as.character(radar),
    datetime = datetime,
    height = heights,
    daterange = c(min(datetime), max(datetime)),
    timesteps = difftimes,
    data = data,
    attributes = list(
      where = data.frame(
        interval = as.integer(interval),
        levels = length(heights),
        height = as.integer(radar_height),
        lon = lon,
        lat = lat
      ),
      how = data.frame(wavelength = wavelength, rcs_bird = rcs, sd_vvp_thresh = sd_vvp_threshold)
    ),
    regular = regular
  )
  class(output) <- "vpts"
  return(output)
}
