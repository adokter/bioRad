#' Deprecated bioRad functions and data
#'
#' The functions and data listed below are deprecated or renamed and will be
#' defunct in the near future. When possible, alternative functions with similar
#' functionality are mentioned.
#'
#' @name bioRad-deprecated
#' @keywords internal
NULL

#' @rdname bioRad-deprecated
#' @returns `TRUE`
#' @export
#' @section check_docker:
#' This function has been removed and always returns TRUE
check_docker <- function(...) {
  warning("check_docker has been deprecated, and always returns TRUE")
  return(TRUE)
}

#' @rdname bioRad-deprecated
#' @returns No return value, called for warning message side effect only
#' @export
#' @section update_docker:
#' This function has been deprecated
update_docker <- function(...) {
  warning("update_docker has been deprecated")
}

#' @rdname bioRad-deprecated
#' @returns an object of class \link{numeric_version}
#' @export
#' @section vol2bird_version:
#' This function has been moved to package vol2birdR
vol2bird_version <- function(...) {
  .Deprecated("vol2birdR::vol2bird_version")
  rlang::check_installed('vol2birdR',format_reason_vol2bird("to run `vol2bird_version`."), version = min_package_version("vol2birdR"))
  warning("vol2bird_version has been moved to package vol2birdR")
  vol2birdR::vol2bird_version()
}

#' @rdname bioRad-deprecated
#' @returns No return value, called for warning message side effect only
#' @export
#' @section download_basemap:
#' This function has been deprecated
#' ggmap has been replaced by ggspatial which no longer requires a pre-downloaded raster basemap
download_basemap <- function(...) {
  warning("download_basemap has been deprecated; ?bioRad::map for details")
  return("cartolight")
}
