#' Check if a file is a vertical profile (`vp`)
#'
#' Checks whether a file is a vertical profile (`vp`) in the ODIM HDF5 format
#' that can be read with bioRad.
#'
#' @param file Character. Path of the file to check.
#'
#' @return `TRUE` for a vertical profile file in readable format, otherwise
#'   `FALSE`.
#'
#' @export
#'
#' @seealso
#' * [read_vpfiles()]
#' * [get_odim_object_type()]
#' * [is.vp()]
#'
#' @examples
#' # Locate the vertical profile example file
#' vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
#'
#' # Check if it is a vpfile
#' is.vpfile(vpfile)
is.vpfile <- function(file) {
  type <- get_odim_object_type(file)
  if (is.na(type)) {
    return(FALSE)
  } else {
    return(type == "VP")
  }
}
