#' Get a quantity from a vertical profile (`vp`) or time series of vertical
#' profiles (`vpts`)
#'
#' Outputs the requested quantity per height bin in a named vector (`vp`), a list of
#' named vectors (list of `vp`s) or a height bin x time matrix (`vpts`). Default
#' quanitity is density. Output for quanities "eta", "dbz", "ff", "u", "v", "w"
#' and "dd" are set to 0, -inf or NaN if sd_vvp is larger than
#' [sd_vvp_threshold()][sd_vvp_threshold()].
#'
#' @param x A `vp`, a list of `vp`s or a `vpts` object.
#' @param quantity A profile quantity, one of:
#'   * `u`
#'   * `v`
#'   * `w`
#'   * `ff`
#'   * `dd`
#'   * `sd_vvp`
#'   * `gap`
#'   * `dbz`
#'   * `eta`
#'   * `dens`
#'   * `DBZH`
#'   * `n`
#'   * `n_all`
#'   * `n_dbz`
#'   * `n_dbz_all`
#'
#' @details This function grabs any of the data quantities stored in
#' [vp][summary.vp] or [vpts][summary.vpts] objects. See the
#' documentation of the vertical profile [vp][summary.vp] class for a
#' description of each of these quantities.
#'
#' @export
#' @examples
#' # load example profile
#' data(example_vp)
#'
#' # extract the animal density ("dens") quantity
#' get_quantity(example_vp, "dens")
get_quantity <- function(x, quantity) {
  UseMethod("get_quantity", x)
}

#' @rdname get_quantity
#' @export
#' @return class `vp`: a named vector for the requested quantity.
get_quantity.vp <- function(x, quantity = "dens") {
  stopifnot(inherits(x, "vp"))
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
#' @export
#' @return class `list`: a list of a named vectors for the requested
#' quantity.
get_quantity.list <- function(x, quantity = "dens") {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("Requires list of vp objects as input.")
  }
  lapply(x, get_quantity.vp, quantity = quantity)
}

#' @rdname get_quantity
#' @export
#' @return class `vpts`: a (height x time) matrix of the
#' requested quantity.
get_quantity.vpts <- function(x, quantity = "dens") {
  ## this function should checkout both the gap and sd_vvp flags
  stopifnot(inherits(x, "vpts"))
  output <- x$data[quantity][[1]]
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
