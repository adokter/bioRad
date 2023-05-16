#' Regularize a time series of vertical profiles (`vpts`) on a
#' regular time grid
#'
#' Projects objects of class `vpts` on a regular time grid
#'
#' Projects objects of class `vpts` on a regular time grid, and fills
#' temporal gaps by nearest neighbor interpolation.
#' @param ts An object inheriting from class `vpts`, see
#' [`vpts()`][summary.vpts] for details.
#' @param interval Time interval grid to project on. When '`auto`' the
#' median interval in the time series is used.
#' @param date_min Start time of the projected time series, as a POSIXct object.
#' Taken from `ts` by default'.
#' @param date_max End time of the projected time series, as a POSIXct object.
#' Taken from `ts` by default.
#' @param units Optional units of `interval` and `fill`, one of 'secs', 'mins',
#' 'hours','days', 'weeks'. Defaults to 'mins'.
#' @param fill Numeric or Logical. fill each regularized timestep with the closest
#' original profile found within a time window of +/- `fill`.
#' When `TRUE`, `fill` maps to `interval`, filling single missing
#' timesteps. When `FALSE`, `fill`  maps to 0, disabling filling.
#' @param verbose Logical, when `TRUE` prints text to console.
#' @param keep_datetime Logical, when `TRUE` keep original radar acquisition timestamps.
#'
#' @return An object of class `vpts` with regular time steps.
#'
#' @export
#'
#' @details Irregular time series of profiles are typically aligned on a
#' regular time grid with the expected time interval at which a radar provides
#' data. Alignment is performed using a nearest neighbor interpolation limited to
#' neighboring profiles that fall within +/- `fill` (centered) of an original profile.
#'
#' In plots of regular time series (see [plot.vpts()]) temporal gaps of
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
#'
#' # regularize the time series on a 10 minute interval grid,
#' # and fill data gaps smaller then 1 hour by nearest neighbor interpolation
#' tsRegular <- regularize_vpts(ts, interval=600, fill=3600)
regularize_vpts <- function(ts, interval = "auto", date_min, date_max,
                            units = "secs", fill = TRUE, verbose = TRUE, keep_datetime = FALSE) {
  assert_that(is.vpts(ts))
  if (interval != "auto") assert_that(is.number(interval), interval > 0) 

  if (!(units %in% c("secs", "mins", "hours", "days", "weeks"))) {
    stop(
      "Invalid 'units' argument. Should be one of",
      "c('secs', 'mins', 'hours','days', 'weeks')"
    )
  }
  if (length(units) > 1) {
    stop("Invalid or missing 'units' argument.")
  }
  assert_that(is.flag(verbose))
  assert_that(is.flag(keep_datetime))

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

  if (is.flag(fill)) {
    # deprecation warning of old fill=TRUE behaviour
    if(fill && !missing(fill)) warning("fill=TRUE behaviour has changed in bioRad version >= 0.6. Use fill=Inf to reproduce the old fill=TRUE result")
    # convert TRUE to dt and FALSE to 0.
    fill=fill*dt
  } else{
    assert_that(is.number(fill), fill>0)
    if(are_equal(fill,Inf)){
      # map infinity to the largest stepsize, to guarantee everything is filled
      fill=max(ts$timesteps)
    }
    fill <- as.difftime(fill, units = units)
  }

  rounding_dt = lubridate::make_difftime(as.numeric(dt,units="secs"))

  if(missing(date_min)) date_min <- tryCatch(lubridate::floor_date(ts$daterange[1],paste(rounding_dt,attr(rounding_dt, "units"))), error = function(e) {ts$daterange[1]})
  if(missing(date_max)) date_max <- tryCatch(lubridate::ceiling_date(ts$daterange[2],paste(rounding_dt,attr(rounding_dt, "units"))), error = function(e) {ts$daterange[2]})

  assert_that(is.time(date_min))
  assert_that(is.time(date_max))
  assert_that(date_max >= date_min)

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
  # Remove interpolated vp's which are further than +/- `fill` of any original vp
  index2 <- which(abs(ts$datetime[index] - grid) > as.double(fill, units = "secs"))
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
