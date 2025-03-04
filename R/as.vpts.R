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

  # Silencing "no visible binding" NOTES
  height <- datetime <- source_file <- radar <- NULL
  bioRad_internal_levels <- bioRad_internal_interval <- bioRad_internal_dummy_index <- bioRad_internal_profile_index <- profile_shape_hash <- NULL

  # 1) sort by datetime and height
  # 2) split out profiles, including those with identical radar and datetime
  # 3) add profile_index as unique profile identifier
  data %>%
    dplyr::arrange(data, datetime, height) %>%
    dplyr::group_by(radar, datetime, height) %>%
    dplyr::mutate(bioRad_internal_dummy_index = dplyr::row_number()) %>%
    dplyr::group_by(radar, datetime, bioRad_internal_dummy_index) %>%
    dplyr::arrange(datetime, bioRad_internal_dummy_index, height) %>%
    dplyr::mutate(bioRad_internal_profile_index = dplyr::cur_group_id()) %>%
    dplyr::group_by(bioRad_internal_profile_index) %>%
    dplyr::select(-bioRad_internal_dummy_index) %>%
    dplyr::mutate(bioRad_internal_interval = height-dplyr::lag(height)) %>%
    dplyr::add_count(name="bioRad_internal_levels") -> data

  # identify profiles with a different profile layer definition than the median
  # and remove them
  data %>%
    dplyr::summarize(profile_shape_hash = as.numeric(paste0(rev(height), collapse = ""))) %>%
    dplyr::filter(profile_shape_hash != stats::median(profile_shape_hash)) %>%
    dplyr::pull(bioRad_internal_profile_index) ->
    profile_index_drop

  if(length(profile_index_drop)>0){
    warning(paste("Profiles found with different altitude layer specifications, keeping only the most
    common one, dropping ",length(profile_index_drop),"profile(s) ..." ))

    # determine unique levels and intervals for all input profiles
    interval_unique <- unique(data$bioRad_internal_interval)
    interval_unique <- interval_unique[!is.na(interval_unique)]
    levels_unique <- unique(data$bioRad_internal_levels)

    # remove selected profiles
    data <- dplyr::filter(data, !(bioRad_internal_profile_index %in% profile_index_drop))

    # provide informative messages on interval and number of layers retained
    interval_median <- stats::median(data$bioRad_internal_interval, na.rm=TRUE)
    levels_median <- stats::median(data$bioRad_internal_levels)
    if(length(interval_unique)>1){
      warning(paste("Profiles found with different altitude interval:",paste(sort(interval_unique),collapse=" ")), ", retaining ",interval_median, " only.")
    }
    if(length(levels_unique)>1){
      warning(paste("Profiles found with different number of height layers:",paste(sort(levels_unique),collapse=" ")), ", retaining ",levels_median, " only.")
    }
  }

  # remove internal columns used for profile separation
  data <-
    dplyr::select(dplyr::ungroup(data), -c(bioRad_internal_interval, bioRad_internal_levels, bioRad_internal_profile_index))

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
  heights <- unique(data[["height"]])
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

  # Check and warn for multiple values of specific attributes and return only the first values of those attributes
  check_multivalue_attributes <- function(data) {
    attributes <- c("radar_longitude", "radar_latitude", "rcs", "sd_vvp_threshold", "radar_wavelength")
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
  wavelength <- first_values$radar_wavelength

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
