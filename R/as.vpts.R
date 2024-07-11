
#' Convert a dataframe into a vpts object
#'
#' @param data a dataframe created from a VPTS CSV file
#' @returns a bioRad vpts object
#' @importFrom glue glue
#' @examples
#' # locate example file in VPTS CSV format:
#' df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))
#' # convert the data.frame to a vpts object:
#' as.vpts(df)
#' @export
as.vpts <- function(data) {
  assertthat::assert_that(inherits(data,"data.frame"))

  # rename alternative names to standard names
  if("dbz_all" %in% names(data)){
    data <- data %>%
      dplyr::rename(DBZH = "dbz_all")
  }

  height <- datetime <- source_file <- radar <- NULL

  # Throw error if nrows per height are not identical 

  assertthat::assert_that(
    remainder_is_zero(dim(data)[1], length(unique(data$height))) > 0,
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

# Check and warn for multiple values of specific attributes and return only the first values of those attributes
check_multivalue_attributes <- function(data) {
  attributes <- c("radar_longitude", "radar_latitude", "rcs", "sd_vvp_threshold")
  first_values <- list()
  for (attr in attributes) {
    if (length(unique(data[[attr]])) > 1) {
      warning(paste0("multiple `", attr, "` values found, storing only first (",
                     as.character(data[[attr]][1]), ") as the functional attribute."))
    }
    first_values[[attr]] <- data[[attr]][1]
  }
    return(first_values)
}

  first_values <- check_multivalue_attributes(data)
  
  # Directly extract and assign values from the list
  lon <- first_values$radar_longitude
  lat <- first_values$radar_latitude
  rcs <- first_values$rcs
  sd_vvp_threshold <- first_values$sd_vvp_threshold
  
  # Convert dataframe
  maskvars <- c("radar", "rcs", "sd_vvp_threshold", "radar_latitude", "radar_longitude", "radar_height", "radar_wavelength", "source_file", "datetime", "height", "sunrise", "sunset", "day")

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
