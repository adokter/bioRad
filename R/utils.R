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
skip_if_no_mistnet <- function() {
  if (rlang::is_installed("vol2birdR")) {
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
#' @keywords internal
check_radar_codes <- function(radars) {
  wrong_codes <- radars[nchar(radars) != 5]
  if (length(wrong_codes) > 0) {
    stop(
      "Radar codes should be 5 characters: ",
      paste(wrong_codes, collapse = ", ")
    )
  } else {
    radars.csv <- utils::read.csv(url("https://lw-enram.s3-eu-west-1.amazonaws.com/radars.csv"))
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
  xy <- data.frame(x = lon, y = lat)
  sp::coordinates(xy) <- c("x", "y")
  sp::proj4string(xy) <- sp::CRS("+proj=longlat +datum=WGS84")

  res <- sp::spTransform(xy, proj4string)

  # Check if the result is a SpatialPointsDataFrame
  if (inherits(res, "SpatialPointsDataFrame")) {
    # If it is, convert it to a SpatialPoints object
    rownames(res@bbox) <- c("x", "y")
    colnames(res@coords) <- c("x", "y")
    res <- sp::SpatialPoints(coords = res@coords, proj4string = res@proj4string, bbox = res@bbox)
  }
  return(res)
}
#' A wrapper for [spTransform()].
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
  } else {
    message("No extension detected; assuming file type .txt which maps to stdout format")
    return("txt")
  }
}

#' Check if a number is a factor of another
#'
#' Determines if a given number is a factor of another number
#' by checking if the remainder is zero after division.
#' @param number The number to be checked if it is a factor.
#' @param factor The potential factor to check against the number.
#' @return A logical value indicating whether the factor is indeed a factor of the number.
#' @keywords internal
#' @noRd
isFactor <- function(number, factor) {
  return(number %% factor == 0)
}

# Extract required fields from frictionless schema
#' @param schema a frictionless schema
#' @returns a character vector of required field names in a VPTS CSV schema
#' @keywords internal
#' @noRd
get_required_fields <- function(schema) {
  required_vars <- sapply(schema$fields, function(x) {
    if (!is.null(x$constraints) && !is.null(x$constraints$required)) {
      return(x$constraints$required)
    } else {
      return(FALSE)
    }
  })
  required_fields <- sapply(schema$fields[required_vars], function(x) x$name)
  return(required_fields)
}

# Recursive function to extract variable names from frictionless schema

#' @param lst the "fields" list of a frictionless schema
#' @returns a character vector of variable names in the order of a VPTS CSV schema
#' @keywords internal
#' @noRd
extract_names <- function(lst) {
  if (is.list(lst)) {
    names <- lapply(lst, function(x) extract_names(x$name))
    unlist(names)
  } else {
    lst
  }
}

#' Convert a tibble into a matrix
#'
#' Reshapes a tibble as an m✕n matrix of m distinct radar heights and
#' n observations (sweeps) ordered by time. Each tibble contains data of a single vpts attribute.
#' @param tibble A tibble in the format: datetime, height, variable, value.
#' @return A list with two elements: the 'variable' of interest and the reshaped matrix
#' @keywords internal
#' @noRd
tibble_to_mat <- function(tibble) {
  unique_heights <- unique(tibble[["height"]])
  matrix_list <- lapply(unique_heights, function(height) {
    height_subset <- tibble[tibble[["height"]] == height, ]
    matrix(height_subset$value, nrow = 1)
  })
  matrix <- do.call(rbind, matrix_list)
  return(list(variable = tibble$variable[1], matrix = matrix))
}

#' Convert a vpts dataframe into an ordered list of matrices
#'
#' @param data A dataframe created from a VPTS CSV file
#' @param maskvars a character vector of radar variables to be masked from the input , e.g., c("radar_latitude", "radar_longitude", ...)
#' @param schema a frictionless schema
#' @returns A named list of matrices ordered according to radvars
#' @keywords internal
#' @noRd
df_to_mat_list <- function(data, maskvars, schema) {
  datetime <- height <- variable <- fields <- NULL
  radvars <- extract_names(schema$fields)
  radvars <- radvars[!radvars %in% maskvars]

  tbls_lst <- data %>%
    dplyr::select(c(setdiff(colnames(data), maskvars), "datetime", "height")) %>%
    tidyr::pivot_longer(-c(datetime, height), names_to = "variable", values_to = "value") %>%
    dplyr::group_by(variable) %>%
    dplyr::group_split()

  unnamed_mat_list <- lapply(tbls_lst, tibble_to_mat)
  var_names <- sapply(unnamed_mat_list, function(x) x$variable)
  named_mat_list <- lapply(unnamed_mat_list, `[[`, "matrix")
  names(named_mat_list) <- var_names
  ordered_mat_list <- named_mat_list[radvars]

  return(ordered_mat_list)
}