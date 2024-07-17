#' Tidyverse methods for bioRad objects
#'
#' Tidyverse methods for bioRad objects, currently only select is implemented. When \code{dplyr} or \code{tidyverse} is loaded the \code{select} method can be used to select parameters.
#' @param .data data object of class \code{scan} or \code{pvol}
#' @param ... other arguments
#' @name tidyverse
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#' # locate example volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # load the file:
#' example_pvol <- read_pvolfile(pvolfile)
#' pvol_selected<-select(example_pvol, DBZH, ZDR) 
#' # Only selected parameters are retained in the pvol
#' get_scan(pvol_selected, 1.5)
#' # Also a series of parameters in a scan can be selected
#' select(get_scan(example_pvol, 2.5), VRADH:ZDR)
#' }
# generalizations for the dplyr verb `select` to `scan` and `pvol` objects
#' @keywords internal
select.scan <- function(.data, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("package dplyr required, please install it first") # nocov
  } #
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("package rlang required, please install it first") # nocov
  } #
  if (!requireNamespace("tidyselect", quietly = TRUE)) {
    stop("package tidyselect required, please install it first") # nocov
  } #
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data$params)
  .data$params <- rlang::set_names(.data$params[pos], names(pos))
  .data
}

#' @keywords internal
select.pvol <- function(.data, ...) {
  .data$scans <- lapply(.data$scans, select.scan, ...)
  .data
}

register_all_s3_methods <- function() {
  # nocov start
  register_s3_method("dplyr", "select", "scan")
  register_s3_method("dplyr", "select", "pvol")
  # nocov end
}

# from: https://github.com/r-spatial/stars/blob/master/R/tidyverse.R
# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
# nocov start
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
