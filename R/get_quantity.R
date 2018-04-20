#' Get a quantity of a vertical profile (\code{vp}) or time series of vertical profiles (\code{vpts})
#'
#' @param x A vp or vpts object.
#' @param quantity A profile quantity, one of:
#' \itemize{
#'  \item{\code{"HGHT"}}{}
#'  \item{\code{"u"}}{}
#'  \item{\code{"v"}}{}
#'  \item{\code{"w"}}{}
#'  \item{\code{"ff"}}{}
#'  \item{\code{"dd"}}{}
#'  \item{\code{"sd_vvp"}}{}
#'  \item{\code{"gap"}}{}
#'  \item{\code{"dbz"}}{}
#'  \item{\code{"eta"}}{}
#'  \item{\code{"dens"}}{}
#'  \item{\code{"DBZH"}}{}
#'  \item{\code{"n"}}{}
#'  \item{\code{"n_all"}}{}
#'  \item{\code{"n_dbz"}}{}
#'  \item{\code{"n_dbz_all"}}{}
#' }
#'
#' @details This function grabs any of the data quantities stored in
#' \link[=summary.vp]{vp} or \link[=summary.vpts]{vpts} objects. See the
#' documentation of the vertical profile \link[=summary.vp]{vp} class for a
#' description of each of these quantities.
#'
#' @export
get_quantity <- function(x, quantity) {
  UseMethod("get_quantity", x)
}

#' @rdname get_quantity
#' @export
#' @return class \code{vp}: a named vector for the requested quantity.
get_quantity.vp <- function(x, quantity = "dens") {
  stopifnot(inherits(x, "vp"))
  output <- x$data[quantity][,1]
  names(output) <- x$data$HGHT

  if (quantity == "eta") {
    output[x$data$sd_vvp < sd_vvp(x)] <- 0
    return(output)
  }
  if (quantity == "dbz") {
    output[x$data$sd_vvp < sd_vvp(x)] <- -Inf
    return(output)
  }
  if (quantity %in% c("ff", "u", "v", "w", "dd")) {
    output[x$data$sd_vvp < sd_vvp(x)] <- NaN
    return(output)
  }
  return(output)
}

#' @rdname get_quantity
#' @export
#' @return class \code{vplist}: a list of a named vectors for the requested
#' quantity.
get_quantity.vplist <- function(x, quantity = "dens") {
  stopifnot(inherits(x, "vplist"))
  lapply(x, get_quantity.vp, quantity = quantity)
}

#' @rdname get_quantity
#' @export
#' @return class \code{vpts}: a (height x time) matrix of the
#' requested quantity.
get_quantity.vpts <- function(x, quantity = "dens") {
  ## this function should checkout both the gap and sd_vvp flags
  stopifnot(inherits(x, "vpts"))
  output <- x$data[quantity][[1]]
  rownames(output) <- x$heights
  colnames(output) <- as.character(x$dates)
  if (quantity == "eta") {
    output[x$data$sd_vvp < rvsd(x)] <- 0
    return(output)
  }
  if (quantity == "dbz") {
    output[x$data$sd_vvp < rvsd(x)] <- -Inf
    return(output)
  }
  if (quantity %in% c("ff", "u", "v", "w", "dd")) {
    output[x$data$sd_vvp < rvsd(x)] <- NaN
    return(output)
  }
  return(output)
}
