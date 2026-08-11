#' Check for NaN values in data frames
#'
#' S3 method for \code{\link[base]{is.nan}} that works with data frames.
#' Identifies cells with \code{NaN} (not a number) in a data frame, extending
#' the base function which only works on vectors.
#'
#' @param x A data.frame object
#' @returns A logical matrix of the same dimensions as \code{x}, with \code{TRUE}
#'   for cells containing \code{NaN} and \code{FALSE} otherwise
#' @exportS3Method base::is.nan
#' @noRd
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, NaN),
#'   b = c(NaN, 5, 6)
#' )
#' is.nan(df)
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}


#' Skip test if missing dependencies for mapping
#'
#' Function map depends on several spatial dependencies (ggspatial, prettymapr, rosm).
#' This helper function allows to skip a test if these dependencies are  not available
#' Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
#' @returns Invisibly returns TRUE if dependencies available, otherwise skips the test with a message "map() dependencies (ggspatial, prettymapr, rosm) not installed".
#' @noRd
skip_if_no_mapping <- function() {
  if (all(sapply(c("ggspatial","prettymapr", "rosm"), requireNamespace, quietly = TRUE))){
     return(invisible(TRUE))
  }
  testthat::skip("map() dependencies (ggspatial, prettymapr, rosm) not installed")
}

#' Skip test if no tidyselect
#'
#' dplyr select method require package tidyselect
#' This helper function allows to skip a test if tidyselect is not available
#' Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
#' @returns Invisibly returns TRUE if tidyselect is available, otherwise skips the test with a message "Package tidyselect not installed".
#' @noRd
skip_if_no_tidyselect <- function() {
  if (requireNamespace("tidyselect", quietly = TRUE)) {
     return(invisible(TRUE))
  }
  testthat::skip("Package tidyselect not installed")
}

#' Skip test if no mistnet
#'
#' Some functions require MistNet to be enabled in package vol2birdR.
#' This helper function allows to skip a test if MistNet is not available, e.g. when running in CI.
#' Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
#' @returns Invisibly returns TRUE if MistNet is available, otherwise skips the test with a message "No MistNet".
#' @noRd
skip_if_no_mistnet <- function() {
  if (rlang::is_installed("vol2birdR", version = "1.3.0", compare = ">=")) {
    if (vol2birdR::mistnet_installed()) {
      return(invisible(TRUE))
    }
  }
  testthat::skip("No MistNet")
}

#' Skip test if vol2birdR not installed
#'
#' Some functions require suggested package vol2birdR to be installed.
#' This helper function allows to skip a test if vol2birdR is not available, e.g. when running in CI.
#' Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
#' @returns Invisibly returns TRUE if vol2birdR is installed, otherwise skips the test with
#' a message "Package vol2birdR not installed".
#' @noRd
skip_if_no_vol2birdR <- function() {
  if (rlang::is_installed("vol2birdR", version = "1.3.0", compare = ">=")) {
    return(invisible(TRUE))
  }
  testthat::skip("Package vol2birdR not installed")
}

#' Check if radar codes are exactly 5 characters
#'
#' @param radars character vector. Radar codes to check, e.g. `c("bejab",
#'   "bewideu")`.
#' @returns NULL. Will stop and show error message if at least one of the
#'   provided radar codes is not exactly 5 characters.
#' @noRd
check_radar_codes <- function(radars) {
  wrong_codes <- radars[nchar(radars) != 5]
  if (length(wrong_codes) > 0) {
    stop(
      "Radar codes should be 5 characters: ",
      paste(wrong_codes, collapse = ", ")
    )
  } else {
    # Load the JSON data from the new URL
    radars.json <- jsonlite::fromJSON("https://raw.githubusercontent.com/enram/aloftdata.eu/main/_data/OPERA_RADARS_DB.json")
    radar_codes = stats::na.omit(radars.json$odimcode)
    wrong_codes <- radars[!(radars %in% radar_codes)]
    if (length(wrong_codes) > 0) {
      stop(
        "Radar codes don't exist: ",
        paste(wrong_codes, collapse = ", ")
      )
    }
  }
}

#' Check if character date is in specific format
#'
#' @param date character. Character representation of a date, e.g.
#'   `"2018-12-13"`.
#' @param format character. strptime format the date should have, e.g.
#'   `"\%Y-\%m-\%d"`
#' @returns NULL. Will stop and show error message if date does not have correct
#'   date format.
#' @noRd
check_date_format <- function(date, format) {
  parsed_date <- as.Date(date, format = format, tz = NULL)
  if (is.na(parsed_date)) {
    stop("Incorrect date format: ", date)
  }
}

#' Transform coordinates with `sf`
#' Converts geographic (WGS84) coordinates to a specified projection
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param proj4string A coordinate reference system understood by [sf::st_crs()].
#' @returns An object of class `sf`.
#' @noRd
wgs_to_proj <- function(lon, lat, proj4string) {
  xy <- sf::st_as_sf(
    data.frame(x = lon, y = lat),
    coords = c("x", "y"),
    crs = 4326
  )
  sf::st_transform(xy, sf::st_crs(proj4string))
}

#' Transform coordinates with `sf`
#' Converts projected coordinates to geographic (WGS84) coordinates.
#'
#' @param x The x-coordinate in the projected system.
#' @param y The y-coordinate in the projected system.
#' @param proj4string A coordinate reference system understood by [sf::st_crs()].
#' @returns An object of class `sf`.
#' @noRd
proj_to_wgs <- function(x, y, proj4string) {
  tryCatch(
    {
      xy <- sf::st_as_sf(
        data.frame(x = x, y = y),
        coords = c("x", "y"),
        crs = sf::st_crs(proj4string)
      )
      sf::st_transform(xy, 4326)
    },
    error = function(err) {
      stop("proj_to_wgs() failed", call. = FALSE)
    }
  )
}
#' Match a set of regular expressions to a list of files
#'
#' Match a set of regular expressions to a list of files and return those
#' filenames that comply to any of the provided regular expressions. This
#' function basically wraps a grep to make it work on vectors by combining the
#' vector of regex options as possible options.
#'
#' @param file_list character vector. Haystack of filenames/filepaths.
#' @param regex_list character vector. Needle of regular expressions to which
#'   filenames should comply.
#' @returns character vector. Subset of filenames from the file_list that comply
#'   to the provided regular expressions in regex_list.
#' @noRd
match_filenames <- function(file_list, regex_list) {
  grep(paste(regex_list, collapse = "|"), file_list, value = TRUE)
}

#' extract strings from a vector using regex, analog to stringr::str_extract
#'
#' @param string Input vector. A character vector.
#' @param pattern Regex pattern to look for
#' @param ... passed on to `regexpr()`
#' @returns A character vector with matches only, possibly of different length as
#'   `string`
#' @noRd
extract_string <- function(string, pattern, ...) {
  regmatches(string,
    m = regexpr(
      pattern = pattern,
      text = string,
      ...
    )
  )
}

#' Guess the file type of a file
#'
#' Guess the file type of a file based on the file signature
#' for HDF5 and gzip files) or the presence of comma separators (for CSV files).
#' More details about HDF5 specification can be found on the [HDF group website](https://docs.hdfgroup.org/hdf5/develop/_f_m_t1.html)
#' If no file type can be inferred, it assumes the file is a text file.
#'
#' @param file_path A character string containing the path to the file
#' @param n_lines An integer, the number of lines to read for guessing a CSV file
#' @returns A character string representing the guessed file type ("h5", "gz", "csv", or "txt")
#' @noRd
guess_file_type <- function(file_path, n_lines = 5) {
  # Check if it's an HDF5 or gzip file by looking at the first few bytes
  first_bytes <- readBin(file_path, "raw", n = 10)

  # HDF5 files have a consistent sigature https://docs.hdfgroup.org/hdf5/develop/_f_m_t1.html
  if (identical(first_bytes[1:8], charToRaw("\211HDF\r\n\032\n"))) {
    return("h5")
  }
  # Gzip files have a consistent magic number 1f 8b
  if (identical(first_bytes[1:2], as.raw(c(0x1f, 0x8b)))) {
    return("gz")
  }
  # If it's not an HDF5 or gzip file, check if it's a CSV file
  first_lines <- readLines(file_path, n = n_lines)

  ## If every line in n_lines contains a comma, assume it's a CSV file
  if (all(sapply(first_lines, function(line) grepl(",", line)))) {
    return("csv")
  }

  if(tools::file_ext(file_path) == "txt"){
    return("txt")
  } else {
    message("No extension detected; assuming file type .txt which maps to stdout format")
    return("txt")
  }
}

#' Identify minimum required package version of dependencies.
#'
#' Identify the minimum required package version of dependencies
#' listed in Suggests or Imports of the DESCRIPTION file.
#'
#' @param pkg A character string with a package name.
#' @returns A character string with the numeric version. When
#' no version is specified or the package is not listed in Suggests
#' or Depends a value `NULL` is returned.
#' @noRd
min_package_version <- function(pkg) {

  assertthat::assert_that(is.character(pkg), length(pkg)==1)

  # 1. Read the "Suggests" field from your package's DESCRIPTION file
  # (Replace "yourPackageName" with your actual package name)
  suggests_field <- utils::packageDescription("bioRad", fields = "Suggests")
  imports_field <- utils::packageDescription("bioRad", fields = "Imports")

  if (is.na(suggests_field) & is.na(imports_field)) {
    warning("No dependencies found in DESCRIPTION.")
  }

  combined_field <- paste0(ifelse(is.na(imports_field),"",paste0(imports_field,",\n")), ifelse(is.na(suggests_field),"",suggests_field))

  # 2. Split the comma-separated dependencies
  deps <- unlist(strsplit(combined_field, ","))

  # 3. Locate the specific package string
  pkg_string <- deps[grep(pkg, deps)]

  if (length(pkg_string) == 0) {
    warning("Package `", pkg, "` is not listed in Imports or Suggests.")
    return(NULL)
  }

  # 4. Extract the version number inside the parentheses (e.g., ">= 1.2.3")
  version_match <- regmatches(pkg_string, regexec("\\(([^)]+)\\)", pkg_string))[[1]]

  if (length(version_match) < 2) {
    warning("Package `", pkg, "` has no minimum version specified.")
    return(NULL) # No minimum version was specified in the DESCRIPTION file
  }

  # Clean up extra spaces and operators (like ">= ") to get just the version string
  clean_version <- gsub("[>=< ]", "", version_match[2])
  return(clean_version)
}
