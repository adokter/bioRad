#' Concatenate vertical profiles (\code{vp}) into a list of vertical profiles
#'
#' @param ... objects of class \code{vp}
#'
#' @return an object of class \code{list}
#'
#' @export
c.vp <- function(...) {
  vps <- list(...)
  vptest <- sapply(vps, function(x) is(x, "vp"))
  if (FALSE %in% vptest) {
    warning("Non-vp objects found!")
    return(vps)
  }
  # extract radar identifiers
  radars <- unique(sapply(vps, '[[', "radar"))
  if (length(radars) > 1) {
    warning("Vertical profiles are not from a single radar!")
  }
  output <- vps
  class(output) <- c("list")
  output
}
