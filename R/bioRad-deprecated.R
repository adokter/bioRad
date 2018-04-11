## bioRad-deprecated.r
#' @title Deprecated functions in package \pkg{bioRad}.
#' @description The functions listed below are deprecated or renamed and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned.
#' @name bioRad-deprecated
#' @keywords internal
NULL

#' @name beamheight-deprecated
#' @rdname bioRad-deprecated
#' @section \code{oldName}:
#' For \code{beamheight}, use \code{\link{beam_height}}.
#' @export
beamheight <- function(range, elev, k = 4/3, lat = 35, re = 6378, rp = 6357) {
  .Deprecated("beam_height")
  beam_height(range, elev, k, lat, re, rp)
}

#' @name beamwidth-deprecated
#' @rdname bioRad-deprecated
#' @section \code{oldName}:
#' For \code{beamwidth}, use \code{\link{beam_width}}.
#' @export
beamwidth <- function(range, angle = 1) {
  .Deprecated("beam_width")
  beam_width(range, angle)
}

#' @name bind-deprecated
#' @rdname bioRad-deprecated
#' @section \code{oldName}:
#' For \code{bind}, use \code{\link{bind_into_vpts}}.
#' @export
bind <- function(x, ...) {
  .Deprecated("bind_into_vpts")
  bind_into_vpts(x, ...)
}


