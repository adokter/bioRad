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
#' vptsfile <- system.file("extdata", "example_vpts.csv", package = "bioRad")
#' read_vpts(vptsfile)
#' # read a single vertical profile file in ODIM h5 format:
#' vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
#' read_vpts(vpfile)
#' # read a vertical profile time series in `vol2bird` stdout format:
#' stdout_file <- system.file("extdata", "example_vpts.txt", package = "bioRad")
#' read_vpts(stdout_file, radar = "KBGM", wavelength = "S")
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
#' @param df If `TRUE` returns data as dataframe rather than `vpts` object.
#' @return `vpts` object.
#' @keywords internal
#' @noRd
read_vpts_csv <- function(files, df = FALSE) {

  if (!exists("cached_schema")) {
    # Read the schema from the URL and cache it
    cached_schema <- jsonlite::fromJSON(system.file("extdata", "vpts-csv-table-schema.json", package = "bioRad"), simplifyDataFrame = FALSE, simplifyVector = TRUE)
    cached_schema$missingValues <- c("", "NA")
  }

    # Create Frictionless Data Package
    package <- frictionless::create_package()
    # Add resource to the package
    package <- frictionless::add_resource(
      package,
      "vpts",
      data = files,
      schema = cached_schema
    )

  # Read resource (compares data with schema and binds rows of all files)
  data <- frictionless::read_resource(package, "vpts")

  # Convert data
  source_file <- datetime <- radar <- NULL

  data <- dplyr::mutate(
    data,
    radar = as.factor(radar),
    source_file = as.factor(source_file),
    datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  # Return data as data frame
  if (df) {
    return(data)
  } else {
    data <- as.vpts(data)
  }

  return(data)
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
