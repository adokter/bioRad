#' Check if a file is a vertical profile (\code{vp})
#'
#' Checker whether a file is a vertical profile that can be read with
#' package \pkg{bioRad}
#'
#' @param filename A string containing a filename
#'
#' @return TRUE when \code{filename} is a vertical profile, otherwise FALSE
#'
#' @export
#'
#' @examples
#' profile <- system.file("extdata", "profile.h5", package = "bioRad")
#' is.vpfile(profile)   #> TRUE
#'
is.vpfile  <- function(filename) {
  type <- get_odim_object_type(filename)
  if (is.na(type)) {
    return(FALSE)
  } else {
    return(type == "VP")
  }
}
