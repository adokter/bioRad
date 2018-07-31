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
#' @return class \code{list}: a list of a named vectors for the requested
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
#' @return class \code{vpts}: a (height x time) matrix of the
#' requested quantity.
get_quantity.vpts <- function(x, quantity = "dens") {
  ## this function should checkout both the gap and sd_vvp flags
  stopifnot(inherits(x, "vpts"))
  output <- x$data[quantity][[1]]
  rownames(output) <- x$heights
  colnames(output) <- as.character(x$dates)
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
