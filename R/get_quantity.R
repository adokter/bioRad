#' Get a quantity from a vertical profile (`vp`) or time series of vertical
#' profiles (`vpts`)
#'
#' Returns values for the selected quantity from a vertical profile (`vp`),
#' list, or time series of vertical profiles (`vpts`). Values are organized per
#' height bin. Values for `eta` are set to `0`, `dbz` to `-Inf` and `ff`, `u`,
#' `v`, `w`, `dd` to `NaN` when the `sd_vvp` for that height bin is below the
#' [sd_vvp_threshold()].
#'
#' @param x A `vp`, list of `vp` or `vpts` object.
#' @param quantity Character. A (case sensitive) profile quantity, one of:
#'   * `height`: Height bin (lower bound) in m above sea level.
#'   * `u`: Speed component west to east in m/s.
#'   * `v`: Speed component south to north in m/s.
#'   * `w`: Vertical speed (unreliable!) in m/s.
#'   * `ff`: Horizontal speed in m/s.
#'   * `dd`: Direction in degrees clockwise from north.
#'   * `sd_vvp`: VVP radial velocity standard deviation in m/s.
#'   * `gap`: Angular data gap detected in T/F.
#'   * `dbz`: Animal reflectivity factor in dBZ.
#'   * `eta`: Animal reflectivity in cm^2/km^3.
#'   * `dens`: Animal density in animals/km^3.
#'   * `DBZH`: Total reflectivity factor (bio + meteo scattering) in dBZ.
#'   * `n`: Number of data points used for the ground speed estimates
#'   (quantities `u`, `v`, `w`, `ff`, `dd`).
#'   * `n_all`: Number of data points used for the radial velocity standard
#'   deviation estimate (quantity `sd_vvp`).
#'   * `n_dbz`: Number of data points used for reflectivity-based estimates
#'   (quantities `dbz`, `eta`, `dens`).
#'   * `n_dbz_all`: Number of data points used for the total reflectivity
#'   estimate (quantity `DBZH`).
#' * `attributes`: List of the vertical profile's `what`, `where` and `how`
#' attributes.
#'
#' @export
#'
#' @seealso
#' * [summary.vp()]
#' * [`sd_vvp_threshold()<-`][sd_vvp_threshold<-] for setting the `sd_vvp`
#' threshold of an object.
#'
#' @examples
#' # Extract the animal density (dens) quantity from a vp object
#' get_quantity(example_vp, "dens")
#'
#' # Extract the horizontal speed (ff) quantity from a vpts object and show the
#' # first two datetimes
#' get_quantity(example_vpts, "ff")[,1:2]
get_quantity <- function(x, quantity) {
  UseMethod("get_quantity", x)
}

#' @rdname get_quantity
#'
#' @export
#'
#' @return For a `vp` object: a named (height bin) vector with values for the
#'   selected quantity.
get_quantity.vp <- function(x, quantity = "dens") {
  stopifnot(inherits(x, "vp"))
  available <- names(x$data)
  assert_that(
    quantity %in% available,
    msg = paste0("Can't find quantity `", quantity, "` in `x`.")
  )
  output <- x$data[quantity][, 1]
  names(output) <- x$data$height

  if (quantity == "eta") {
    output[x$data$sd_vvp < sd_vvp_threshold(x)] <- 0
    return(output)
  }
  if (quantity == "dbz") {
    output[x$data$sd_vvp < sd_vvp_threshold(x)] <- -Inf
    return(output)
  }
  if (quantity %in% c("ff", "u", "v", "w", "dd")) {
    output[x$data$sd_vvp < sd_vvp_threshold(x)] <- NaN
    return(output)
  }
  return(output)
}

#' @rdname get_quantity
#'
#' @export
#'
#' @return For a `list` object: a list of named (height bin) vectors with values
#'   for the selected quantity.
get_quantity.list <- function(x, quantity = "dens") {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("`x` must be list of vp objects.")
  }
  lapply(x, get_quantity.vp, quantity = quantity)
}

#' @rdname get_quantity
#'
#' @export
#'
#' @return For a `vpts` object: a (height bin * datetime) matrix with values for
#'   the selected quantity.
get_quantity.vpts <- function(x, quantity = "dens") {
  ## this function should checkout both the gap and sd_vvp flags
  stopifnot(inherits(x, "vpts"))
  assert_that(
    quantity %in% c(names(x$data), "height"),
    msg = paste0("Can't find quantity `", quantity, "` in `x`.")
  )
  if(quantity == "height"){
    output <- matrix(rep(as.numeric(x$height),dim(x)[1]), ncol=dim(x)[1])
  } else{
    output <- x$data[quantity][[1]]
  }
  rownames(output) <- x$height
  colnames(output) <- as.character(x$datetime)

  if (quantity == "eta") {
    output[x$data$sd_vvp < sd_vvp_threshold(x)] <- 0
    return(output)
  }
  if (quantity == "dbz") {
    output[x$data$sd_vvp < sd_vvp_threshold(x)] <- -Inf
    return(output)
  }
  if (quantity %in% c("ff", "u", "v", "w", "dd")) {
    output[x$data$sd_vvp < sd_vvp_threshold(x)] <- NaN
    return(output)
  }
  return(output)
}
