#' Identify `NaN` in a dataframe
#'
#' Identify cells with `NaN` (not a number) in a data frame. Improves on the
#' default[base::is.nan()] function, which only works on vectors, by allowing
#' data frames as input.
#'
#' @param x A `data.frame` object.
#'
#' @return A matrix of the same dimension as `x`, with `TRUE`/`FALSE` values for
#'   whether each cell in the original data frame is a number or not.
#' @keywords internal
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

#' Skip test if no aws.s3
#'
#' Some functions require package aws.s3
#' This helper function allows to skip a test if aws.s3 is not available
#' Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
#' @return Invisibly returns TRUE if aws.s3 is available, otherwise skips the test with a message "Package aws.s3 not installed".
#' @keywords internal
skip_if_no_aws.s3 <- function() {
  if (requireNamespace("aws.s3", quietly = TRUE)) {
     return(invisible(TRUE))
  }
  testthat::skip("Package aws.s3 not installed")
}

#' Skip test if missing dependencies for mapping
#'
#' Function map depends on several spatial dependencies (ggspatial, prettymapr, rosm).
#' This helper function allows to skip a test if these dependencies are  not available
#' Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
#' @return Invisibly returns TRUE if dependencies available, otherwise skips the test with a message "map() dependencies (ggspatial, prettymapr, rosm) not installed".
#' @keywords internal
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
#' @return Invisibly returns TRUE if tidyselect is available, otherwise skips the test with a message "Package tidyselect not installed".
#' @keywords internal
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
#' @return Invisibly returns TRUE if MistNet is available, otherwise skips the test with a message "No MistNet".
#' @keywords internal
skip_if_no_mistnet <- function() {
  if (requireNamespace("vol2birdR", quietly = TRUE)) {
    if (vol2birdR::mistnet_exists() && file.exists(file.path(vol2birdR::torch_install_path(),"data","mistnet_nexrad.pt"))) {
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
#' @return Invisibly returns TRUE if vol2birdR is installed, otherwise skips the test with
#' a message "Package vol2birdR not installed".
#' @keywords internal
skip_if_no_vol2birdR <- function() {
  if (requireNamespace("vol2birdR", quietly = TRUE)) {
    return(invisible(TRUE))
  }
  testthat::skip("Package vol2birdR not installed")
}

#' Check if radar codes are exactly 5 characters
#'
#' @param radars character vector. Radar codes to check, e.g. `c("bejab",
#'   "bewideu")`.
#'
#' @return NULL. Will stop and show error message if at least one of the
#'   provided radar codes is not exactly 5 characters.
#' @keywords internal
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
#'
#' @return NULL. Will stop and show error message if date does not have correct
#'   date format.
#'
#' @keywords internal
check_date_format <- function(date, format) {
  parsed_date <- as.Date(date, format = format, tz = NULL)
  if (is.na(parsed_date)) {
    stop("Incorrect date format: ", date)
  }
}


#' A wrapper for [sp::spTransform()].
#' Converts geographic (WGS84) coordinates to a specified projection
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param proj4string An object of class 'CRS', as defined in package `sp`.
#'
#' @keywords internal
#'
#' @return An object of class `SpatialPoints`.
wgs_to_proj <- function(lon, lat, proj4string) {
  xy<-sf::st_as_sf(data.frame(x = lon, y = lat), coords=c('x','y'), crs=4326L)
  res <- sf::st_transform(xy, proj4string)

  res <- sf::as_Spatial(res)
  rownames(res@bbox) <- c("x", "y")
  colnames(res@coords) <- c("x", "y")

  return(res)
}

#' A wrapper for [sp::spTransform()].
#' Converts projected coordinates to geographic (WGS84) coordinates.
#'
#' @param x The x-coordinate in the projected system.
#' @param y The y-coordinate in the projected system.
#' @param proj4string An object of class 'CRS', as defined in package `sp`.
#'
#' @keywords internal
#'
#' @return An object of class `SpatialPoints`.
proj_to_wgs <- function(x, y, proj4string) {
  xy <- data.frame(lon = x, lat = y)
  sp::coordinates(xy) <- c("lon", "lat")
  sp::proj4string(xy) <- proj4string
  res <- NULL

  # Catch error when rgdal is not installed and sp_evolution_status is set to 0
  tryCatch(
    {
      res <- sp::spTransform(xy, sp::CRS("+proj=longlat +datum=WGS84"))

      # Check if the result is a SpatialPointsDataFrame
      if (inherits(res, "SpatialPointsDataFrame")) {
        # If it is, convert it to a SpatialPoints object and correct names
        rownames(res@bbox) <- c("lon", "lat")
        colnames(res@coords) <- c("lon", "lat")
        res <- sp::SpatialPoints(coords = res@coords, proj4string = res@proj4string, bbox = res@bbox)
      }
      return(res)
    },
    error = function(err) {
      if (grepl("package rgdal is required", err$message)) {
        err <- simpleError("spTransform failed. Try resetting sp_evolution_status: sp::set_evolution_status(2L)")
      } else {
        err <- simpleError("proj_to_wgs() failed")
      }
      stop(err)
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
#'
#' @return character vector. Subset of filenames from the file_list that comply
#'   to the provided regular expressions in regex_list.
#' @keywords internal
match_filenames <- function(file_list, regex_list) {
  grep(paste(regex_list, collapse = "|"), file_list, value = TRUE)
}

#' extract strings from a vector using regex, analog to stringr::str_extract
#'
#' @param string Input vector. A character vector.
#' @param pattern Regex pattern to look for
#' @param ... passed on to `regexpr()`
#'
#' @return A character vector with matches only, possibly of different length as
#'   `string`
#' @keywords internal
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
#' @return A character string representing the guessed file type ("h5", "gz", "csv", or "txt")
#' @keywords internal
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

#' Check if remainder of a division is zero
#'
#' Determines if a given number is a factor of another number
#' by checking if the remainder is zero after division.
#' @param number The number to be divided (the dividend)
#' @param divisor The divisor to check against the number.
#' @return A logical value indicating whether the remainder of the division is zero
#' @keywords internal
#' @noRd
remainder_is_zero <- function(number, divisor) {
  return(number %% divisor == 0)
}
