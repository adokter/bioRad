#' Convert a vertical profile (`vp`) or time series of vertical profiles
#' (`vpts`) to a data frame
#'
#' Converts a vertical profile (`vp`) or a time series of vertical profiles
#' (`vpts`) to a data frame containing all quantities per datetime and height.
#' Has options to include latitude/longitude/antenna height (parameter `geo`)
#' and day/sunrise/sunset (parameter `suntime`).
#'
#' @param x A `vp` or `vpts` object.
#' @param row.names `NULL` or a character vector giving the row names for the
#'   data frame. Missing values are not allowed. See [base::as.data.frame()].
#' @param optional Logical. If `FALSE` then the names of the variables in the
#'   data frame are checked to ensure that they are syntactically valid variable
#'   names and are not duplicated. See [base::as.data.frame()].
#' @param geo Logical. When `TRUE`, adds latitude (`lat`), longitude (`lon`) and
#'   antenna height of the radar (`height_antenna`) to each row.
#' @param suntime Logical. When `TRUE`, adds whether it is daytime (`day`) and
#'   the datetime of `sunrise` and `sunset` to each row.
#' @param lat Numeric. Radar latitude in decimal degrees. When set, overrides
#'   the latitude stored in `x` for [sunrise()]/[sunset()] calculations.
#' @param lon Numeric. Radar longitude in decimal degrees. When set, overrides
#'   the longitude stored in `x` for [sunrise()]/[sunset()] calculations.
#' @param elev Numeric. Sun elevation in degrees, used for
#'   [sunrise()]/[sunset()] calculations.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return A `data.frame` object, containing radar, datetime and height as rows
#'   and all profile quantities as columns, complemented with some oft-used
#'   additional information (columns `lat`, `lon`, `height_antenna`, `day`,
#'   `sunrise`, `sunset`).
#'
#' @export
#'
#' @seealso
#' * [summary.vpts()]
#'
#' @details
#' Note that only the `dens` quantity is thresholded for radial velocity
#' standard deviation by [sd_vvp_threshold()]. This is different from the
#' default [plot.vp()], [plot.vpts()] and [get_quantity()] functions, where
#' quantities `eta`, `dbz`, `ff`, `u`, `v`, `w`, `dd` are all thresholded by
#' [sd_vvp_threshold()].
#'
#' @examples
#' # Convert vp object to a data.frame
#' vp_df <- as.data.frame(example_vp)
#'
#' # Print data.frame
#' vp_df
#'
#' # Convert vpts object to a data.frame
#' vpts_df <- as.data.frame(example_vpts)
#'
#' # Print the first 5 rows of the data.frame
#' vpts_df[1:5, ]
#'
#' # Do not add lat/lon/height_antenna information
#' vpts_df <- as.data.frame(example_vpts, geo = FALSE)
#'
#' # Do not add day/sunrise/sunset information
#' vpts_df <- as.data.frame(example_vpts, suntime = FALSE)
#'
#' # Override the latitude/longitude information stored in the object when
#' # calculating sunrise/sunset information
#' vpts_df <- as.data.frame(example_vpts, lat = 50, lon = 4)
as.data.frame.vp <- function(x, row.names = NULL, optional = FALSE, geo = TRUE,
                             suntime = TRUE, lat = NULL, lon = NULL,
                             elev = -0.268, ...) {
  # check input parameters
  stopifnot(inherits(x, "vp"))
  assertthat::assert_that(assertthat::is.flag(optional))
  assertthat::assert_that(assertthat::is.flag(geo))
  assertthat::assert_that(assertthat::is.flag(suntime))
  # fetch lat and long if missing
  if (is.null(lat)) {
    lat <- x$attributes$where$lat
  }
  if (is.null(lon)) {
    lon <- x$attributes$where$lon
  }
  # coerce data to a data frame
  output <- as.data.frame(x$data, optional = optional, ...)
  # set row.names
  if (!is.null(row.names)) {
    if (is.character(row.names) & length(row.names) ==
      length(x$data$height)) {
      rownames(output) <- row.names
    } else {
      stop(glue::glue(
        "`row.names` must be a character vector of length ",
        "{length(x$data$height)}."
      ))
    }
  }
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
  # override the lat, lon attributes in case of user-provided values
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

#' @rdname as.data.frame.vp
#'
#' @export
as.data.frame.vpts <- function(x, row.names = NULL, optional = FALSE,
                               geo = TRUE, suntime = TRUE, lat = NULL,
                               lon = NULL, elev = -0.268, ...) {
  stopifnot(inherits(x, "vpts"))
  if (is.null(lat)) {
    lat <- x$attributes$where$lat
  }
  if (is.null(lon)) {
    lon <- x$attributes$where$lon
  }
  # coerce data to a data frame
  output <- as.data.frame(lapply(x$data, c), optional = optional, ...)
  # set row.names
  if (!is.null(row.names)) {
    if (is.character(row.names) & length(row.names) ==
      length(x$datetime) * length(x$height)) {
      rownames(output) <- row.names
    } else {
      stop(glue::glue(
        "`row.names` must be a character vector of length ",
        "{length(x$datetime) * length(x$height)}."
      ))
    }
  }
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
  # override the lat, lon attributes in case of user-provided values
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
