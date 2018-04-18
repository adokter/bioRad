#' Bind vertical profiles (vp) into time series (vpts)
#'
#' Binds vertical profiles (\code{vp}) into a vertical profile time series
#' (\code{vpts}), sorted in time. Can also bind multiple \code{vpts} of a
#' single radar into one \code{vpts}.
#'
#' @param x A \code{vp}, \code{vpts} or a vector of these.
#' @param ... A \code{vp}, \code{vpts} or a vector of these.
#'
#' @return A \code{vpts} (see \link{summary.vpts} for details) for a single
#' radar or a list of \code{vpts} for multiple radars. Input \code{vp} are
#' sorted in time in the output \code{vpts}.
#'
#' @export
#'
#' @examples
#' # load the example vertical profile time series (vpts):
#' data(VPTS)
#' # split the vpts into two separate time series,
#' # one containing profile 1-10, and a second containing profile 11-20:
#' vpts1 <- VPTS[1:10]
#' vpts2 <- VPTS[11:20]
#' # use bind_into_vpts to bind the two together:
#' vpts1and2 <- bind_into_vpts(vpts1, vpts2)
#' # verify that the binded vpts now has 20 profiles, 10 from vpts1
#' # and 10 from vpts2:
#' summary(vpts1and2)
#' # extract two profiles:
#' vp1 <- VPTS[1]
#' vp1
#' vp2 <- VPTS[2]
#' vp2
#' # bind the two profiles back into a vpts:
#' bind_into_vpts(vp1, vp2)
bind_into_vpts <- function(x, ...) UseMethod("bind_into_vpts", x)

#' @describeIn bind_into_vpts Bind multiple \code{vp} into a \code{vpts}.
#' If \code{vp} for multiple radars are provided, a list is returned containing
#' a \code{vpts} for each radar.
#'
#' @export
bind_into_vpts.vp <- function(...) {
  vps <- list(...)
  vptest <- sapply(vps, function(x) is(x, "vp"))
  if (FALSE %in% vptest) {
    stop("requires vp objects as input")
  }
  # extract radar identifiers
  radars <- unique(sapply(vps, '[[', "radar"))
  vpts(c.vp(...))
}

#' @describeIn bind_into_vpts Bind multiple \code{vplist} into a \code{vpts}.
#' If data for multiple radars is provided, a list is returned containing
#' a \code{vpts} for each radar.
#'
#' @export
bind_into_vpts.vplist <- function(x, ...) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires vplist object as input")
  }
  vpts(x, ...)
}

#' @describeIn bind_into_vpts Bind multiple \code{vpts} into a single
#' \code{vpts}. Requires the input \code{vpts} to be from the same radar.
#'
#' @param attributes_from numeric. Which \code{vpts} to copy attributes from (default:
#' first).
#'
#' @export
bind_into_vpts.vpts <- function(..., attributes_from = 1) {
  vptss <- list(...)
  vptstest <- sapply(vptss,function(x) is(x, "vpts"))
  if (FALSE %in% vptstest) {
    stop("requires vpts objects as input")
  }
  # extract radar identifiers
  radars <- unique(sapply(vptss, '[[', "radar"))
  if (length(radars) > 1) {
    stop("Vertical profiles are not from a single radar")
  }
  if (length(unique(lapply(vptss, '[[', "heights"))) > 1) {
    stop("Vertical profiles have non-aligning altitude layers")
  }
  if (length(unique(lapply(vptss, function(x) names(x$"data")))) > 1) {
    stop("Vertical profiles have different quantities")
  }
  # extract date-times
  dates <- .POSIXct(do.call("c", lapply(vptss, '[[', "dates")), tz = "UTC")
  quantities <- names(vptss[[1]]$data)
  ordering <- order(dates)
  dates <- dates[ordering]
  data <- lapply(quantities,
                 function(quantity) {
                   do.call(cbind, lapply(vptss,
                                         function(x) {
                                           x$"data"[[quantity]]
                                           }))[, ordering]
                   })
  names(data) <- quantities
  difftimes <- difftime(dates[-1],dates[-length(dates)], units = "secs")
  if (length(unique(difftimes)) == 1) {
    regular = TRUE
  } else {
    regular = FALSE
  }
  output <- list(radar = radars, dates = dates, heights = vptss[[1]]$heights,
                 daterange = .POSIXct(c(min(dates), max(dates)), tz = "UTC"),
                 timesteps = difftimes, data = data,
                 attributes = vptss[[attributes_from]]$attributes,
                 regular = regular)
  class(output) = "vpts"
  output
}
