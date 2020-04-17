#' Convert legacy bioRad objects to current version
#'
#' Convert legacy bioRad objects (vp, vpts) that have become
#' obsolete and make them compatible with the current bioRad version.
#'
#' @param x A `vp`, or `vpts` object.
#'
#' @return An updated object of the same class as the input.
#'
#' @export
#'
#' @examples
#' # convert a vp object:
#' convert_legacy(example_vp)
#'
#' # convert a vpts object:
#' convert_legacy(example_vpts)
convert_legacy <- function(x) {
  UseMethod("convert_legacy", x)
}

#' @rdname convert_legacy
#'
#' @export
convert_legacy.vp <- function(x) {
  assert_that(inherits(x, "vp"))
  names(x$data)=sub("HGHT","height",names(x$data))
  x
}

#' @rdname convert_legacy
#'
#' @export
convert_legacy.vpts <- function(x) {
  assert_that(inherits(x, "vpts"))
  names(x)=sub("heights","height",names(x))
  names(x)=sub("dates","datetime",names(x))
  x
}
