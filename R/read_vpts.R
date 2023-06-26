#' Read time series of vertical profiles (`vpts`) from file(s)
#'
#' Reads `vpts` data from one or more files.
#' The following file formats are supported (but cannot be mixed):
#' - [VPTS CSV](https://aloftdata.eu/vpts-csv/).
#' - [ODIM bird profile](https://github.com/adokter/vol2bird/wiki/ODIM-bird-profile-format-specification).
#' - vol2bird standard output (see example below).
#' @param files Path(s) to one or more files containing vpts data.
#' @param ... Additional arguments for backward compatibility, passed to `read_stdout`.
#' @return `vpts` object.
#' @family read functions
#' @export
#' @examples
#' ## read a vertical profile time series in VPTS CSV format:
#' # vptsfile <- system.file("extdata", "example_vpts.csv", package = "bioRad")
#' # read_vpts(vptsfile)
#' # read a single vertical profile file in ODIM h5 format:
#' vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
#' read_vpts(vpfile)
#' # read a vertical profile time series in `vol2bird` stdout format:
#' # stdoutfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")
#' # read_vpts(stdout_file, radar = "KBGM", wavelength = "S")
read_vpts <- function(files, ...) {
  # Define valid extensions
  valid_extensions <- c("csv", "gz", "h5", "txt")

  # Get file extension
  extension <- unique(tools::file_ext(files))

  assertthat::assert_that(
    length(extension) == 1,
    msg = "`files` must all have the same extension."
  )

  # If the file has an extension, check if it is valid
  if (extension != "") {
    assertthat::assert_that(
      extension %in% valid_extensions,
      msg = glue::glue(
        "`files` must have one of the following extensions: {valid_extensions_collapse}",
        valid_extensions_collapse = glue::glue_collapse(valid_extensions, sep = ", ")
      )
    )
    # infer the file type
    guessed_file_type <- guess_file_type(files[1])
    
    assertthat::assert_that(
      extension == guessed_file_type,
      msg = glue::glue(
        "The extension of the input file(s) {extension} does not match the guessed file type: {guessed_file_type}"
      )
    )

  } else {
    # If the file does not have an extension, infer the file type
    extension <- guess_file_type(files)
  }

  # Check if the input file has a .txt extension and if so reroute to read_stdout
  if (extension == "txt") {
    warning(".txt extenstion detected - falling back to read_stdout().\n
    Please consider updating your workflow by using VPTS csv or h5 input files")
    return(do.call(read_stdout, c(list(file = files), list(...))))
  }

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
  schema <- jsonlite::fromJSON(schema, simplifyDataFrame = FALSE, simplifyVector = TRUE)
  schema$missingValues <- c("", "NA")

  package <- frictionless::add_resource(
    package,
    "vpts",
    data = files,
    schema = schema
  )
  
  #Remove NaN from vector of missing values
  #package$resources[[1]]$schema$missingValues <- c("", "NA") 
  
  # Read resource (compares data with schema and binds rows of all files)
  data <- frictionless::read_resource(package, "vpts")


  # Convert data
  source_file <- NULL
  data <- dplyr::mutate(
    data,
    radar = as.factor(radar),
    source_file = as.factor(source_file),
    datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
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
  heights <- unique(data$height)

  #Subset timestamps by first sampled height
  datetime <- data[data$height == heights[1],]$datetime

  #Determine regularity
  difftimes <- difftime(datetime[-1], datetime[-length(datetime)], units = "secs")
  if (length(unique(difftimes)) == 1) {
    regular <- TRUE
  } else {
    regular <- FALSE
  }

  # Get attributes
  radar_height = data$radar_height[1]
  interval <- unique(heights[-1] - heights[-length(heights)])
  wavelength <- unique(data$radar_wavelength)
  lon <- data$radar_longitude[1]
  lat <- data$radar_latitude[1]
  rcs <-  data$rcs[1]
  sd_vvp_threshold <- data$sd_vvp_threshold[1]

  # Convert dataframe
  radvars <- c("ff", "dbz", "dens", "u", "v", "gap", "w", "n_dbz", "dd", "n", "dbz_all", "n_dbz_all", "eta", "sd_vvp", "n_all")
  data <- df_to_mat_list(data, radvars)

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
        interval = interval,
        levels = length(heights),
        height = radar_height,
        lon = lon,
        lat = lat
      ),
      how = data.frame(wavelength = wavelength, rcs_bird = rcs, sd_vvp_thresh = sd_vvp_threshold)
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
