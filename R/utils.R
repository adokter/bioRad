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
#'
#' @keywords internal
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

#' Skip test if no mistnet
#'
#' Some functions require MistNet to be enabled in package vol2birdR.
#' This helper function allows to skip a test if MistNet is not available, e.g. when running in CI.
#' Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
#'
#' @keywords internal
skip_if_no_mistnet <- function(){
  if(rlang::is_installed('vol2birdR')){
    if (vol2birdR::mistnet_exists()) {
      return(invisible(TRUE))
    }
  }
  testthat::skip("No MistNet")
}

#' Check if radar codes are exactly 5 characters
#'
#' @param radars character vector. Radar codes to check, e.g. `c("bejab",
#'   "bewideu")`.
#'
#' @return NULL. Will stop and show error message if at least one of the
#'   provided radar codes is not exactly 5 characters.
#'
#' @keywords internal
check_radar_codes <- function(radars) {
  wrong_codes <- radars[nchar(radars) != 5]
  if (length(wrong_codes) > 0) {
    stop(
      "Radar codes should be 5 characters: ",
      paste(wrong_codes, collapse = ", ")
    )
  } else {
    radars.csv <- read.csv(url("https://lw-enram.s3-eu-west-1.amazonaws.com/radars.csv"))
    wrong_codes <- radars[!(radars %in% radars.csv$countryradar)]
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

#' A wrapper for [spTransform()].
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param proj4string An object of class 'CRS', as defined in package `sp`.
#'
#' @keywords internal
#'
#' @return An object of class `SpatialPoints`.
wgs_to_proj <- function(lon, lat, proj4string) {
  xy <- data.frame(x = lon, y = lat)
  coordinates(xy) <- c("x", "y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, proj4string)
  return(res)
}

#' A wrapper for [spTransform()].
#'
#' @param x Longitude
#' @param y Latitude
#' @param proj4string An object of class 'CRS', as defined in package `sp`.
#' @keywords internal
#' @return An object of class `SpatialPoints`.
proj_to_wgs <- function(x, y, proj4string) {
  xy <- data.frame(lon = x, lat = y)
  coordinates(xy) <- c("lon", "lat")
  proj4string(xy) <- proj4string
  res <- spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
  return(res)
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
extract_string <- function(string,pattern,...) {
  regmatches(string,
             m = regexpr(
               pattern = pattern,
               text = string,
               ...
             ))
}
