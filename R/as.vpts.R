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

  # if dbz_all is a column name, rename to bioRad naming DBZH
  if("dbz_all" %in% names(data)){
    data <- data %>%
      dplyr::rename(DBZH = "dbz_all")
  }
 
  validate_vpts(data)

  # sort by datetime and height
  data |> dplyr::arrange(datetime, height) -> data

  height <- datetime <- source_file <- radar <- NULL

  # Throw error if nrows per height are not identical
  # FIXME: first if statement is a weak check that could fail, could be improved.
  # retaining for now because of speed
  if(!remainder_is_zero(dim(data)[1], length(unique(data$height)))){
    data %>%
      dplyr::group_by(radar, datetime) %>%
      dplyr::mutate(bioRad_internal_interval = height-lag(height)) %>%
      dplyr::add_count(name="bioRad_internal_levels") -> data
    interval_median <- median(data$bioRad_internal_interval, na.rm=TRUE)
    interval_unique <- unique(data$bioRad_internal_interval)
    interval_unique <- interval_unique[!is.na(interval_unique)]
    if(length(interval_unique)>1){
      warning(paste("profiles found with different altitude interval:",paste(sort(interval_unique),collapse=" ")), ", retaining ",interval_median, " only.")
      data <- dplyr::filter(data, bioRad_internal_interval == interval_median)
    }
    levels_median <- median(data$bioRad_internal_levels)
    levels_unique <- unique(data$bioRad_internal_levels)
    if(length(levels_unique)>1){
      warning(paste("profiles found with different number of height layers:",paste(sort(levels_unique),collapse=" ")), ", retaining ",levels_median, " only.")
      data <- dplyr::filter(data, bioRad_internal_levels == levels_median)
    }
    data <- dplyr::select(data, -c(bioRad_internal_interval, bioRad_internal_levels))
  }

  radar <- unique(data[["radar"]])

  # Check radar is unique
  assertthat::assert_that(
    length(radar) == 1,
    msg = "`data` must contain data of a single radar."
  )

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
        warning(paste0("multiple ", as.character(substitute(attr))," values found, storing only first (",
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

  # column names not to store in the vpts$data slot
  radvars_exclude <- c("radar","datetime","height","rcs","sd_vvp_threshold","radar_latitude","radar_longitude","radar_height","radar_wavelength")
  # radvars to include in vpts$data slot
  radvars <- names(data)[!names(data) %in% radvars_exclude]

  # calculate number of vertical profiles present
  n_vp <- nrow(data)/length(heights)

  # cast data.frame to list of matrices
  data_output <- lapply(radvars, function(x) matrix(data[[x]],ncol=n_vp))
  names(data_output)=radvars

  # List of vectors to check
  vectors_to_check <- list(heights = heights, interval = interval, radar_height = radar_height, lon = lon, lat = lat)

  # Identify empty vectors
  empty_vectors <- names(vectors_to_check)[sapply(vectors_to_check, function(v) length(v) == 0)]

  # Stop execution if any empty vectors are found
  if (length(empty_vectors) > 0) {
      stop("Empty vectors detected: ", paste(empty_vectors, collapse=", "))
  }

  # Create vpts object
  output <- list(
    radar = as.character(radar),
    datetime = datetime,
    height = heights,
    daterange = c(min(datetime), max(datetime)),
    timesteps = difftimes,
    data = data_output,
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
