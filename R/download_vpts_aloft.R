#' Title
#'
#' @param date_min
#' @param date_max
#' @param radars
#' @param directory
#' @param overwrite
#' @param format
#' @param source
#'
#' @return
#' @export
#'
#' @examples
download_vpts_aloft <- function(date_min = NULL,
                 date_max = NULL,
                 radars = NULL,
                 directory = ".",
                 overwrite = FALSE,
                 format = "csv", # also hdf5
                 source = "baltrad" # also ecog-04003
                 ) {

# Assertations ------------------------------------------------------------

  # Ensure directory exists
  assert_that(dir.exists(directory),
              msg = glue::glue("path {directory} doesn't exist"))

  # Stop if radar codes are not exactly 5 characters
  assert_that(is.character(radar),
              msg = "radar needs to be a character of length 1")
  assert_that(length(radar) == 1,
              msg = paste0("radar is not of length 1"))

  # Stop if dates are not date and not
  assert_that(lubridate::is.POSIXt(date_min), msg = "date_min is not a date")
  assert_that(lubridate::is.POSIXt(date_max), msg = "date_max is not a date")
  assert_that(date_min <= date_max,
              msg = "date_max is not greater or equal to date_min")

  # Stop if overwrite is not a logical
  assert_that(is.logical(overwrite), msg = "overwrite is not a logical")
}
