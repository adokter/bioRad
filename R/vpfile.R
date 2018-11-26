#' Check if a local file is a vertical profile (\code{vp})
#'
#' Checker whether a file is a vertical profile that can be read with
#' package \pkg{bioRad}
#'
#' @param file A string containing a filename
#' @param filename Deprecated argument, use file instead.
#'
#' @return TRUE when \code{filename} is a vertical profile, otherwise FALSE
#'
#' @export
#'
#' @examples
#' profile <- system.file("extdata", "profile.h5", package = "bioRad")
#' is.vpfile(profile) # > TRUE
is.vpfile <- function(file, filename = NULL) {

  # deprecate function arguments
  if (!missing(filename)) {
    warning("argument filename is deprecated; please use file instead.",
      call. = FALSE
    )
    file <- filename
  }

  type <- get_odim_object_type(file)
  if (is.na(type)) {
    return(FALSE)
  } else {
    return(type == "VP")
  }
}
