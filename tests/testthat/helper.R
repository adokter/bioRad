# Some functions require Docker. This helper function allows to skip a test if
# the Docker container is not available, e.g. when running in CI.
# Inspired by <https://testthat.r-lib.org/articles/skipping.html#helpers>.
skip_if_no_docker <- function() {
  if (check_docker(verbose = FALSE) == 0) {
    return(invisible(TRUE))
  }
  testthat::skip("No docker")
}
