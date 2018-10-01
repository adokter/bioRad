#' Regularize a time series of vertical profiles (\code{vpts}) on a
#' regular time grid
#'
#' Projects objects of class \code{vpts} on a regular time grid
#'
#' @param ts An object inhereting from class \code{vpts}, see
#' \code{\link[=summary.vpts]{vpts}} for details.
#' @param interval Time interval grid to project on. When '\code{auto}' the
#' median interval in the time series is used.
#' @param date_min Start time of the projected time series, as a POSIXct object.
#' Taken from \code{ts} when '\code{auto}'.
#' @param date_max End time of the projected time series, as a POSIXct object.
#' Taken from \code{ts} when '\code{auto}'.
#' @param units Optional units of \code{interval}, one of 'secs', 'mins',
#' 'hours','days', 'weeks'. Defaults to 'mins'.
#' @param fill Logical, whether to fill missing timesteps with the values of
#' the closest neighbouring profile.
#' @param verbose Logical, when \code{TRUE} prints text to console.
#'
#' @return An object of class \code{vpts} with regular time steps.
#'
#' @export
#'
#' @details Irregular time series of profiles are typically aligned on a
#' regular time grid with the expected time interval at which a radar provides
#' data. Empty profiles with only missing data values will be inserted at
#' time stamps of the regular time grid that have no matching profile in the
#' irregular time series.
#' 
#' In plots of regular time series (see \code{\link{plot.vpts}}) temporal gaps of
#' missing profiles (e.g. due to radar down time) become visible. In irregular
#' time series data points in the plot are carried through until the time series
#' continues, and temporal data gaps are filled up visually.
#'
#' @examples
#' # start form example vpts object:
#' ts <- example_vpts
#' # regularize the time series on a 5 minute interval grid
#' tsRegular <- regularize_vpts(ts, interval = 5)
regularize_vpts <- function(ts, interval = "auto", date_min = ts$daterange[1],
                            date_max = ts$daterange[2], units = "mins",
                            fill = FALSE, verbose = TRUE) {
  stopifnot(inherits(ts, "vpts"))
  stopifnot(inherits(date_min, "POSIXct"))
  stopifnot(inherits(date_max, "POSIXct"))

  if (!(units %in% c("secs", "mins", "hours", "days", "weeks"))) {
    stop(
      "Invalid 'units' argument. Should be one of",
      "c('secs', 'mins', 'hours','days', 'weeks')"
    )
  }
  if (interval != "auto" && !is.numeric(interval)) {
    stop("Invalid or missing 'interval' argument. Should be a numeric value.")
  }
  if (length(units) > 1) {
    stop("Invalid or missing 'units' argument.")
  }
  if (!is.logical(fill) || length(fill) > 1) {
    stop("Fill argument should be a logical value.")
  }

  if (interval == "auto") {
    dt <- as.difftime(median(ts$timesteps), units = "secs")
    if (verbose) {
      cat(paste("projecting on", dt, "seconds interval grid...\n"))
    }
  } else {
    dt <- as.difftime(interval, units = units)
  }
  daterange <- c(date_min, date_max)
  grid <- seq(from = daterange[1], to = daterange[2], by = dt)
  index <- sapply(
    grid,
    function(x) {
      which.min(abs(ts$dates - x))
    }
  )
  quantity.names <- names(ts$data)
  ts$data <- lapply(
    1:length(ts$data),
    function(x) {
      ts$data[[x]][, index]
    }
  )
  if (!fill) {
    index2 <- which(abs(ts$dates[index] - grid) > as.double(dt, units = "secs"))
    if (length(index2) > 0) {
      ts$data <- lapply(
        1:length(ts$data),
        function(x) {
          tmp <- ts$data[[x]]
          tmp[, index2] <- NA
          tmp
        }
      )
    }
  }
  names(ts$data) <- quantity.names
  ts$dates <- grid
  ts$timesteps <- rep(as.double(dt, units = "secs"), length(grid) - 1)
  ts$regular <- TRUE
  return(ts)
}
