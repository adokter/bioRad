#' Convert legacy bioRad objects
#'
#' Convert legacy bioRad objects (`vp`, `vpts`) and make them compatible with
#' the current bioRad version. Conversion includes renaming `HGHT` to `height`.
#'
#' @param x A `vp` or `vpts` object.
#'
#' @return An updated object of the same class as the input.
#'
#' @export
#'
#' @seealso
#' * [summary.vp()]
#' * [summary.vpts()]
#'
#' @examples
#' # Convert a vp object
#' vp <- convert_legacy(example_vp)
#'
#' # Convert a vpts object
#' vpts <- convert_legacy(example_vpts)
convert_legacy <- function(x) {
  UseMethod("convert_legacy", x)
}

#' @rdname convert_legacy
#'
#' @export
convert_legacy.vp <- function(x) {
  assertthat::assert_that(inherits(x, "vp"))
  names(x$data) <- sub("HGHT", "height", names(x$data))
  x
}

#' @rdname convert_legacy
#'
#' @export
convert_legacy.vpts <- function(x) {
  assertthat::assert_that(inherits(x, "vpts"))
  names(x) <- sub("heights", "height", names(x))
  names(x) <- sub("dates", "datetime", names(x))
  x
}
