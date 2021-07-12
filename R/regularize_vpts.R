#' Regularize a time series of vertical profiles (\code{vpts}) on a
#' regular time grid
#'
#' Projects objects of class \code{vpts} on a regular time grid
#'
#' @param ts An object inheriting from class \code{vpts}, see
#' \code{\link[=summary.vpts]{vpts}} for details.
#' @param interval Time interval grid to project on. When '\code{auto}' the
#' median interval in the time series is used.
#' @param date_min Start time of the projected time series, as a POSIXct object.
#' Taken from \code{ts} by default'.
#' @param date_max End time of the projected time series, as a POSIXct object.
#' Taken from \code{ts} by default.
#' @param units Optional units of \code{interval}, one of 'secs', 'mins',
#' 'hours','days', 'weeks'. Defaults to 'mins'.
#' @param fill Logical, whether to fill missing timesteps with the values of
#' the closest neighboring profile (i.e., further than
#' +/- half the interval to the closest neighboring profile).
#' @param verbose Logical, when \code{TRUE} prints text to console.
#' @param keep_datetime Logical, when \code{TRUE} keep original radar acquisition timestamps.
#'
#' @return An object of class \code{vpts} with regular time steps.
#'
#' @export
#'
#' @details Irregular time series of profiles are typically aligned on a
#' regular time grid with the expected time interval at which a radar provides
#' data. Alignment is performed with a nearest neighbor interpolation limited to
#' neighboring profiles within +/- half the interval duration. If `fill` is
#' true, all profiles are considered for the interpolation.
#'
#' In plots of regular time series (see \code{\link{plot.vpts}}) temporal gaps of
#' missing profiles (e.g. due to radar down time) become visible. In irregular
#' time series data points in the plot are carried through until the time series
#' continues, and temporal data gaps are filled up visually.
#'
#' @examples
#' # start form example vpts object:
#' data(example_vpts)
#' ts <- example_vpts
#'
#' # regularize the time series on a 5 minute interval grid
#' tsRegular <- regularize_vpts(ts, interval = 300)
regularize_vpts <- function(ts, interval = "auto", date_min, date_max,
                            units = "secs", fill = FALSE, verbose = TRUE, keep_datetime = FALSE) {
  if (!inherits(ts, "vpts")) {
    stop("ts arguement should be a vpts object.")
  }

  if (!(units %in% c("secs", "mins", "hours", "days", "weeks"))) {
    stop(
      "Invalid 'units' argument. Should be one of",
      "c('secs', 'mins', 'hours','days', 'weeks')"
    )
  }
  if (interval != "auto" && !(is.numeric(interval) && interval > 0) ) {
    stop("Invalid or missing 'interval' argument. Should be a strictly positive numeric value.")
  }
  if (length(units) > 1) {
    stop("Invalid or missing 'units' argument.")
  }
  if (!is.logical(fill) || length(fill) > 1) {
    stop("Fill argument should be a logical value.")
  }
  if (!is.logical(verbose) || length(verbose) > 1) {
    stop("verbose argument should be a logical value.")
  }
  if (!is.logical(keep_datetime) || length(keep_datetime) > 1) {
    stop("keep_datetime argument should be a logical value.")
  }

  # remove profiles with duplicate timestamps:
  index_duplicates <- which(ts$timesteps == 0) + 1
  if (length(index_duplicates) > 0) {
    warning(paste("Dropped", length(index_duplicates), "profiles with duplicate datetime values"))
    ts <- ts[-index_duplicates]
  }

  if (interval == "auto") {
    dt <- as.difftime(median(ts$timesteps), units = "secs")
    if (verbose) {
      cat(paste("projecting on", dt, "seconds interval grid...\n"))
    }
  } else {
    dt <- as.difftime(interval, units = units)
  }

  rounding_dt = lubridate::make_difftime(as.numeric(dt,units="secs"))

  if(missing(date_min)) date_min <- tryCatch(lubridate::floor_date(ts$daterange[1],paste(rounding_dt,attr(rounding_dt, "units"))), error = function(e) {ts$daterange[1]})
  if(missing(date_max)) date_max <- tryCatch(lubridate::ceiling_date(ts$daterange[2],paste(rounding_dt,attr(rounding_dt, "units"))), error = function(e) {ts$daterange[2]})

  if (!inherits(date_min, "POSIXct")){
    stop("date_min argument should be a POSIXct object.")
  }
  if (!inherits(date_max, "POSIXct")){
    stop("date_max argument should be a POSIXct object.")
  }
  if (!(date_max >= date_min)){
    stop("date_max should be greater than date_min.")
  }

  daterange <- c(date_min, date_max)
  grid <- seq(from = daterange[1], to = daterange[2], by = dt)

  index <- data.table::setDT(data.frame(datetime=ts$datetime))[data.frame(grid), roll = "nearest", which = TRUE, on = "datetime==grid"]

  quantity.names <- names(ts$data)
  ts$data <- lapply(
    1:length(ts$data),
    function(x) {
      ts$data[[x]][, index]
    }
  )
  index2 <- integer(0)
  if (!fill) {
    index2 <- which(abs(ts$datetime[index] - grid) > as.double(dt, units = "secs") / 2)
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
  ts$daterange <- daterange
  ts$timesteps <- rep(as.double(dt, units = "secs"), length(grid) - 1)
  if(!keep_datetime){
    ts$datetime <- grid
  } else{
    ts$datetime <- ts$datetime[index]
  }
  ts$regular <- TRUE
  return(ts)
}
