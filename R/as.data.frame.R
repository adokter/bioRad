#' Convert a vertical profile (\code{vp}) to a Data Frame
#'
#' Converts a vertical profile to a Data Frame, and optionally adds information
#' on sunrise/sunset, day/night and derived quantities like migration
#' traffic rates.
#'
#' @param x An object of class \code{vp}.
#' @param row.names \code{NULL} or a character vector giving the row names for
#' the data frame. Missing values are not allowed. See [base::as.data.frame()].
#' @param optional If \code{FALSE} then the names of the variables in the data
#' frame are checked to ensure that they are syntactically valid variable names
#' and are not duplicated.
#' @param quantities An optional character vector with the names of the
#' quantities to include as columns in the data frame.
#' @param elev Sun elevation in degrees, see \link{sunrise}/\link{sunset}.
#' @param lat Radar latitude in decimal degrees. When set, overrides the
#' latitude stored in \code{x} in \link{sunrise}/\link{sunset} calculations
#' @param lon Radar longitude in decimal degrees. When set, overrides the
#' longitude stored in \code{x} in \link{sunrise}/\link{sunset} calculations.
#' @param suntime Logical, when \code{TRUE}, adds sunrise/sunset and day/night
#' information to each row.
#' @param geo Logical, when \code{TRUE}, adds latitude, longitude and antenna
#' height of the radar to each row.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return An object of class \code{data.frame}.
#'
#' @export
#'
#' @details
#' Note that only the "dens" quantity is thresholded for radial velocity
#' standard deviation by \link{sd_vvp_threshold}. Note that this is different from the
#' default \link{plot.vp}, \link{plot.vpts} and \link{get_quantity.vp}
#' functions, where quantities "eta", "dbz", "ff", "u", "v", "w", "dd" are
#' all thresholded by \link{sd_vvp_threshold}
#'
#' @examples
#' # Load the example vertical profile
#' vp <- example_vp
#'
#' # Convert to a data.frame
#' vp_df <- as.data.frame(vp)
#'
#' # Print data.frame
#' vp_df
#'
#' # Do not compute sunrise/sunset information
#' vp_df <- as.data.frame(vp, suntime = FALSE)
#'
#' # Override the latitude/longitude information stored in the object when
#' # calculating sunrise/sunset information
#' vp_df <- as.data.frame(vp, suntime = TRUE, lat = 50, lon = 4)
as.data.frame.vp <- function(x, row.names = NULL, optional = FALSE,
                             quantities = names(x$data), suntime = TRUE,
                             geo = TRUE, elev = -0.268, lat = NULL,
                             lon = NULL, ...) {
  stopifnot(inherits(x, "vp"))
  if (!is.null(row.names)) {
    if (is.character(row.names) & length(row.names) ==
        length(x$datetime) * length(x$height)) {
      rownames(output) <- row.names
    } else {
      stop(paste(
        "`row.names` is not a character vector of length",
        length(x$datetime) * length(x$height)
      ))
    }
  }
  if (is.null(lat)) {
    lat <- x$attributes$where$lat
  }
  if (is.null(lon)) {
    lon <- x$attributes$where$lon
  }
  missing <- which(!(quantities %in% names(x$data)))
  if (length(missing) > 0) {
    stop(paste(
      paste(quantities[missing], collapse = " "),
      "not an available quantity, select one or more of",
      paste(names(x$data), collapse = ",")
    ))
  }
  # coerce data to a data frame
  output <- as.data.frame(x$data, optional = optional, ...)
  # add height and datetime as a column
  output <- cbind(datetime = x$datetime, height = output$height, output)
  output$height <- NULL
  # add radar name
  output <- cbind(radar = x$radar, output, stringsAsFactors = FALSE)
  # add location information
  if (geo) {
    output$lat <- lat
    output$lon <- lon
    output$height_antenna <- x$attributes$where$height
  }
  # override the lat,lon attributes in case of user-provided values
  x$attributes$where$lat <- lat
  x$attributes$where$lon <- lon
  # add day
  if (suntime) {
    dayQ <- !check_night(x, elev = elev)
    dayQ <- c(t(replicate(nrow(x), dayQ)))
    output <- cbind(output, day = dayQ)
    sunrise <- sunrise(x$datetime, lat = lat, lon = lon)
    sunset <- sunset(x$datetime, lat = lat, lon = lon)
    output$sunrise <- as.POSIXct(
      c(t(replicate(nrow(x), sunrise))),
      origin = "1970-1-1", tz = "UTC"
    )
    output$sunset <- as.POSIXct(
      c(t(replicate(nrow(x), sunset))),
      origin = "1970-1-1", tz = "UTC"
    )
  }
  output
}

#' Convert a time series of vertical profiles (\code{vpts}) to a data frame
#'
#' Converts vertical profile time series (objects of class \code{vpts}) to a
#' data Frame, and optionally adds information on sunrise/sunset, day/night
#' and derived quantities like migration traffic rates.
#'
#' @param x An object of class \code{vpts}.
#' @param row.names \code{NULL} or a character vector giving the row names for
#' the data frame. Missing values are not allowed.
#' @param optional If \code{FALSE} then the names of the variables in the data
#' frame are checked to ensure that they are syntactically valid variable names
#' and are not duplicated.
#' @param quantities An optional character vector with the names of the
#' quantities to include as columns in the data frame.
#' @param elev Sun elevation in degrees, see \link{sunrise}/\link{sunset}.
#' @param lat Radar latitude in decimal degrees. When set, overrides the
#' latitude stored in \code{x} in \link{sunrise}/\link{sunset} calculations.
#' @param lon Radar longitude in decimal degrees. When set, overrides the
#' longitude stored in \code{x} in \link{sunrise}/\link{sunset} calculations.
#' @param suntime Logical, when TRUE, adds sunrise/sunset and day/night
#' information to each row.
#' @param geo Logical, when TRUE, adds latitude, longitude and antenna height
#' of the radar to each row.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return An object of class data.frame.
#'
#' @export
#'
#' @details
#' Note that only the 'dens' quantity is thresholded for radial velocity
#' standard deviation by \link{sd_vvp_threshold}. Note that this is different from the
#' default \link{plot.vp}, \link{plot.vpts} and \link{get_quantity.vp}
#' functions, where quantities "eta", "dbz", "ff", "u", "v", "w", "dd" are all
#' thresholded by \link{sd_vvp_threshold}.
#'
#' @examples
#' # Load the example time series of vertical profiles
#' vpts <- example_vpts
#'
#' # Convert to a data.frame
#' vpts_df <- as.data.frame(vpts)
#'
#' # Print the first 10 rows of the data.frame
#' vpts_df[1:10, ]
#'
#' # Do not compute sunrise/sunset information
#' vpts_df <- as.data.frame(vpts, suntime = FALSE)
#'
#' # Override the latitude/longitude information stored in the object when
#' # calculating sunrise/sunset information
#' vpts_df <- as.data.frame(vpts, suntime = TRUE, lat = 50, lon = 4)
as.data.frame.vpts <- function(x, row.names = NULL, optional = FALSE,
                               quantities = names(x$data), suntime = TRUE,
                               geo = TRUE, elev = -0.268, lat = NULL,
                               lon = NULL, ...) {
  stopifnot(inherits(x, "vpts"))
  if (!is.null(row.names)) {
    if (is.character(row.names) & length(row.names) ==
        length(x$datetime) * length(x$height)) {
      rownames(output) <- row.names
    } else {
      stop(paste(
        "'row.names' is not a character vector of length",
        length(x$datetime) * length(x$height)
      ))
    }
  }
  if (is.null(lat)) {
    lat <- x$attributes$where$lat
  }
  if (is.null(lon)) {
    lon <- x$attributes$where$lon
  }
  missing <- which(!(quantities %in% names(x$data)))
  if (length(missing) > 0) {
    stop(paste(
      paste(quantities[missing], collapse = " "),
      "not an available quantity, select one or more of",
      paste(names(x$data), collapse = ",")
    ))
  }
  # coerce data to a data frame
  output <- as.data.frame(lapply(x$data[quantities], c),
                          optional = optional, ...
  )
  # add height and datetime as a column
  output <- cbind(
    datetime = as.POSIXct(
      c(t(replicate(length(x$height), x$datetime))),
      origin = "1970-1-1", tz = "UTC"
    ),
    height = rep(x$height, length(x$datetime)), output
  )
  # add radar name
  output <- cbind(radar = x$radar, output, stringsAsFactors = FALSE)
  # add location information
  if (geo) {
    output$lat <- lat
    output$lon <- lon
    output$height_antenna <- x$attributes$where$height
  }
  # override the lat,lon attributes in case of user-provided values
  x$attributes$where$lat <- lat
  x$attributes$where$lon <- lon
  # add day
  if (suntime) {
    dayQ <- !check_night(x, elev = elev)
    dayQ <- c(t(replicate(length(x$height), dayQ)))
    output <- cbind(output, day = dayQ)
    sunrise <- sunrise(x$datetime, lat = lat, lon = lon)
    sunset <- sunset(x$datetime, lat = lat, lon = lon)
    output$sunrise <- as.POSIXct(
      c(t(replicate(length(x$height), sunrise))),
      origin = "1970-1-1", tz = "UTC"
    )
    output$sunset <- as.POSIXct(
      c(t(replicate(length(x$height), sunset))),
      origin = "1970-1-1", tz = "UTC"
    )
  }
  output
}
