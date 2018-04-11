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
#' @section \code{beamheight}:
#' For \code{beamheight}, use \code{\link{beam_height}}.
#' @export
beamheight <- function(range, elev, k = 4/3, lat = 35, re = 6378, rp = 6357) {
  .Deprecated("beam_height")
  beam_height(range, elev, k, lat, re, rp)
}

#' @name beamwidth-deprecated
#' @rdname bioRad-deprecated
#' @section \code{beamwidth}:
#' For \code{beamwidth}, use \code{\link{beam_width}}.
#' @export
beamwidth <- function(range, angle = 1) {
  .Deprecated("beam_width")
  beam_width(range, angle)
}

#' @name bind-deprecated
#' @rdname bioRad-deprecated
#' @section \code{bind}:
#' For \code{bind}, use \code{\link{bind_into_vpts}}.
#' @export
bind <- function(x, ...) {
  .Deprecated("bind_into_vpts")
  bind_into_vpts(x, ...)
}

#' @name vol2bird-deprecated
#' @rdname bioRad-deprecated
#' @section \code{vol2bird}:
#' For \code{vol2bird}, use \code{\link{calculate_vp}}.
#' @export
vol2bird <- function(){
  .Deprecated("calculate_vp")
  calculate_vp()
}

#' @name night-deprecated
#' @rdname bioRad-deprecated
#' @section \code{night}:
#' For \code{night}, use \code{\link{check_night}}.
#' @export
night <- function(x, ..., elev=-0.268){
  .Deprecated("check_night")
  check_night(x, ..., elev = -0.268)
}

#' @name day-deprecated
#' @rdname bioRad-deprecated
#' @section \code{day}:
#' For \code{day}, use \code{\link{check_night}}.
#' @export
day <- function(x, elev = -0.268){
  .Deprecated("check_night",
              msg = paste("'day' is deprecatedand, its functionality is",
                          "replaced by the 'check_night' function",
                          "(FALSE <-> TRUE)"))
  !check_night(x, elev = -0.268)
}

