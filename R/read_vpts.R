#' Read time series of vertical profiles (`vpts`) from file(s)
#'
#' Reads `vpts` data from one or more files.
#' The following file formats are supported (but cannot be mixed):
#' - [VPTS CSV](https://aloftdata.eu/vpts-csv/).
#' - [ODIM bird profile](https://github.com/adokter/vol2bird/wiki/ODIM-bird-profile-format-specification).
#' @param files Path(s) to one or more files containing vpts data.
#' @param radar Radar (deprecated, use `read_stdout()` instead).
#' @param lat Latitude (deprecated, use `read_stdout()` instead).
#' @param lon Longitude (deprecated, use `read_stdout()` instead).
#' @param height Height (deprecated, use `read_stdout()` instead).
#' @return `vpts` object.
#' @family read functions
#' @export
read_vpts <- function(files, radar = NULL, lat = NULL, lon = NULL, height = NULL) {
  # Check if any of the old parameters are used
  if (!is.null(radar) | !is.null(lat) | !is.null(lon) | !is.null(height)) {
    .Deprecated("read_stdout")
    return(read_stdout(file = files, radar = radar, lat = lat, lon = lon, height = height, wavelength = "C", sep = ""))
  }

  # Get file extension
  extension <- unique(tools::file_ext(files))
  assertthat::assert_that(
    length(extension) == 1,
    msg = "`files` must all have the same extension."
  )

  # Read files
  data <- switch(extension,
    csv = read_vpts_csv(files),
    gz = read_vpts_csv(files),
    h5 = read_vpts_hdf5(files)
  )

  data
}

#' Read time series of vertical profiles (`vpts`) from VPTS CSV file(s)
#'
#' @inheritParams read_vpts
#' @param df If `TRUE` returns data as data frame rather than `vpts`
#'   object.
#' @return `vpts` object.
#' @noRd
read_vpts_csv <- function(files, df = FALSE) {
  # Create Frictionless Data Package
  package <- frictionless::create_package()
  schema <- "https://raw.githubusercontent.com/enram/vpts-csv/main/vpts-csv-table-schema.json"
  package <- frictionless::add_resource(
    package,
    "vpts",
    data = files,
    schema = schema
  )

  # Read resource (compares data with schema and binds rows of all files)
  data <- frictionless::read_resource(package, "vpts")

  # Convert data
  data <- dplyr::mutate(
    data,
    radar = as.factor(radar),
    source_file = as.factor(source_file)
  )

  # Return data as data frame
  if (df) {
    return(data)
  }

  # The following steps convert the data to a vpts object
  # Check radar is unique
  radar <- unique(data$radar)
  assertthat::assert_that(
    length(radar) == 1,
    msg = "`files` must contain data of a single radar."
  )

  # Check whether time series is regular
  datetime <- unique(data$datetime)
  difftimes <- difftime(datetime[-1], datetime[-length(datetime)], units = "secs")
  if (length(unique(difftimes)) == 1) {
    regular <- TRUE
  } else {
    regular <- FALSE
  }

  # TODO: finish following code (inspired by read_stdout)
  # Get attributes
  heights <- unique(data$height)
  interval <- unique(heights[-1] - heights[-length(heights)])
  wavelength <- unique(data$radar_wavelength)

  # Create object
  output <- list(
    radar = radar,
    datetime = datetime,
    height = heights,
    daterange = c(min(datetime), max(datetime)),
    # timesteps = difftimes,
    data = data,
    attributes = list(
      where = data.frame(
        # interval = interval
        # levels = length(heights)
      ),
      how = data.frame(wavelength = wavelength)
    ),
    regular = regular
  )
  class(output) <- "vpts"
  output
}

#' Read time series of vertical profiles (`vpts`) from hdf5 file(s)
#'
#' @inheritParams read_vpts
#' @return `vpts` object.
#' @noRd
read_vpts_hdf5 <- function(files) {
  vps <- read_vpfiles(files)
  bind_into_vpts(vps)
}
