#' Identify `NaN` in a dataframe
#'
#' Identify cells with `NaN` (not a number) in a data frame. Improves on the
#' default[base::is.nan()] function, which only works on vectors, by allowing
#' data frames as input.
#'
#' @param x A `data.frame` object.
#'
#' @return A matrix of the same dimension as `x`, with `TRUE`/`FALSE` values
#' for whether each cell in the original data frame is a number or not.
#'
#' @keywords internal
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

#' Skip test if no docker
#'
#' Some functions require Docker. This helper function allows to skip a test if
#' the Docker container is not available, e.g. when running in CI.
#' Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
#'
#' @keywords internal
skip_if_no_docker <- function() {
  if (check_docker(verbose = FALSE) == 0) {
    return(invisible(TRUE))
  }
  skip("No docker")
}
