#' Look up day of year (doy) or night of year (noy)
#'
#' Returns the day of year (`doy`) or night of year (`noy`) number for datetimes
#' and various bioRad objects. The first night of the year is the night with
#' datetime Jan 01 00:00:00 in the local time zone, so sunset on Jan 1 occurs
#' on the second night of the year and New Years Eve on Dec 31 occurs on the
#' first night of the new year.
#'
#' @param x A `pvol`, `vp`, `vpts`, or `vpi` object, or a [base::as.POSIXct]
#'   datetime.
#' @param lon Numeric. Longitude in decimal degrees.
#' @param lat Numeric. Latitude in decimal degrees.
#' @param method Method by which to do the time zone lookup. Either `fast`
#'   (default) or `accurate`. See [lutz::tz_lookup_coords]].
#' @param ... Optional lat, lon arguments.
#'
#' @name doy_noy
#'
#' @examples
#' # Get day of year of a vp object
#' noy(example_vp)
#'
#' # Get night of year of a vp object
#' noy(example_vp)
#'
#' # Get night of year of a vpts object
#' noy(example_vpts)
NULL

#' @rdname doy_noy
#'
#' @export
doy <- function(x, ..., method = "fast") {
  UseMethod("doy", x)
}

#' @rdname doy_noy
#'
#' @export
noy <- function(x, ..., method = "fast") {
  UseMethod("noy", x)
}

#' @rdname doy_noy
#'
#' @export
doy.default <- function(x, lon, lat, ..., method = "fast") {
  tzone = lutz::tz_lookup_coords(lat, lon, method = method, warn = FALSE)
  as.integer(lubridate::yday(lubridate::with_tz(x, tzone = tzone)))
}

#' @rdname doy_noy
#'
#' @export
noy.default <- function(x, lon, lat, ..., method = "fast") {
  doy.default(x + 12 * 3600, lon, lat, method = method)
}

#' @rdname doy_noy
#'
#' @export
doy.vp <- function(x, ..., method = "fast") {
  stopifnot(inherits(x, "vp"))
  doy(x$datetime, x$attributes$where$lon, x$attributes$where$lat, method = method)
}

#' @rdname doy_noy
#'
#' @export
noy.vp <- function(x, ..., method = "fast") {
  stopifnot(inherits(x, "vp"))
  noy(x$datetime, x$attributes$where$lon, x$attributes$where$lat, method = method)
}

#' @rdname doy_noy
#'
#' @export
doy.vpts <- function(x, ..., method = "fast") {
  stopifnot(inherits(x, "vpts"))
  doy(x$datetime, x$attributes$where$lon, x$attributes$where$lat, method = method)
}

#' @rdname doy_noy
#'
#' @export
noy.vpts <- function(x, ..., method = "fast") {
  stopifnot(inherits(x, "vpts"))
  noy(x$datetime, x$attributes$where$lon, x$attributes$where$lat, method = method)
}

#' @rdname doy_noy
#'
#' @export
doy.vpi <- function(x, ..., method = "fast") {
  stopifnot(inherits(x, "vpi"))
  doy(x$datetime, attributes(x)$lon, attributes(x)$lat, method = method)
}

#' @rdname doy_noy
#'
#' @export
noy.vpi <- function(x, ..., method = "fast") {
  stopifnot(inherits(x, "vpi"))
  noy(x$datetime, attributes(x)$lon, attributes(x)$lat, method = method)
}

#' @rdname doy_noy
#'
#' @export
doy.pvol <- function(x, ..., method = "fast") {
  stopifnot(inherits(x, "pvol"))
  doy(x$datetime, x$attributes$where$lon, x$attributes$where$lat, method = method)
}

#' @rdname doy_noy
#'
#' @export
noy.pvol <- function(x, ...,  method = "fast") {
  stopifnot(inherits(x, "pvol"))
  noy(x$datetime, x$attributes$where$lon, x$attributes$where$lat, method = method)
}
