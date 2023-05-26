#' Deprecated bioRad functions and data
#'
#' The functions and data listed below are deprecated or renamed and will be
#' defunct in the near future. When possible, alternative functions with similar
#' functionality are mentioned.
#'
#' @name bioRad-deprecated
#' @keywords internal
NULL

#' @section check_docker:
#' This function has been removed and always returns TRUE
#'
#' @rdname bioRad-deprecated
#' @export
check_docker <- function(...) {
  warning("check_docker has been deprecated, and always returns TRUE")
  return(TRUE)
}

#' @section update_docker:
#' This function has been deprecated
#'
#' @rdname bioRad-deprecated
#' @export
update_docker <- function(...) {
  warning("update_docker has been deprecated")
}

#' @section vol2bird_version:
#' This function has been moved to package vol2birdR
#'
#' @rdname bioRad-deprecated
#' @export
vol2bird_version <- function(...) {
  .Deprecated("vol2birdR::vol2bird_version")
  rlang::check_installed('vol2birdR',format_reason_vol2bird("to run `vol2bird_version`."))
  warning("vol2bird_version has been moved to package vol2birdR")
  vol2birdR::vol2bird_version()
}

