#' Read time series of vertical profiles (`vpts`) from file(s)
#'
#' Reads `vpts` data from one or more files.
#' The following file formats are supported (but cannot be mixed):
#' - [VPTS CSV](https://aloftdata.eu/vpts-csv/).
#' - [ODIM bird profile](https://github.com/adokter/vol2bird/wiki/ODIM-bird-profile-format-specification).
#' - vol2bird standard output (see example below).
#' @param files Path(s) to one or more files containing vpts data.
#' @param data_frame When `FALSE` (default) output a `vpts` object, when `TRUE` output a data.frame
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
read_vpts <- function(files, data_frame = FALSE, ...) {
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

        # Attempt to call read_stdout
    tryCatch({
      return(do.call(read_stdout, c(list(file = files), list(...))))
  },
        error = function(e) {
        # Display custom message
        message(paste(e$message, " See ?read_stdout() for more details."))
        stop()
      }
   )
  }

  # Read files
  data <- switch(extension,
    csv = read_vpts_csv(files, data_frame=data_frame),
    gz = read_vpts_csv(files, data_frame=data_frame),
    h5 = read_vpts_hdf5(files, data_frame=data_frame)
  )
  data
}

#' Read time series of vertical profiles (`vpts`) from VPTS CSV file(s)
#'
#' @inheritParams read_vpts
#' @param data_frame If `TRUE` returns data as dataframe rather than `vpts` object.
#' @return `vpts` object.
#' @keywords internal
#' @noRd
read_vpts_csv <- function(files, data_frame = FALSE) {

  #suppressMessages(
  data <- readr::read_csv(files, show_col_types = FALSE)
  #,
  #col_types = readr::cols(
  #   .default = readr::col_guess(),  
  #   `...1` = readr::col_skip()      # Skip unnamed columns
  #  ))
  #)

  #Validate the data
  validate_vpts(data)

  # Convert data
  source_file <- datetime <- radar <- NULL

  data <- dplyr::mutate(
    data,
    datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  # Return data as data frame
  if (!data_frame) {
    data <- as.vpts(data)
  }

  return(data)
}
#' Read time series of vertical profiles (`vpts`) from hdf5 file(s)
#'
#' @inheritParams read_vpts
#' @return `vpts` object.
#' @noRd
read_vpts_hdf5 <- function(files, data_frame = FALSE) {
  vps <- read_vpfiles(files)
  output <- bind_into_vpts(vps)
  if(data_frame) output <- as.data.frame(output)
  output
}
