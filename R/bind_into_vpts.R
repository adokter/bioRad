#' Bind vertical profiles (\code{vp}) into time series (\code{vpts})
#'
#' Binds vertical profiles (\code{vp}) into a vertical profile time series
#' (\code{vpts}), sorted in time. Can also bind multiple \code{vpts} of a
#' single radar into one \code{vpts}.
#'
#' @param x A \code{vp}, \code{vpts} or a vector of these.
#' @param ... A \code{vp}, \code{vpts} or a vector of these.
#'
#' @return A \code{\link[=summary.vpts]{vpts}} for a single radar or a list of
#' \code{vpts} for multiple radars. Input \code{vp} are sorted in time in the
#' output \code{vpts}.
#'
#' @export
#'
#' @examples
#' # load example time series of vertical profiles:
#' data(example_vpts)
#'
#' # split the vpts into two separate time series, one containing profile 1-10,
#' # and a second containing profile 11-20:
#' vpts1 <- example_vpts[1:10]
#' vpts2 <- example_vpts[11:20]
#'
#' # use bind_into_vpts to bind the two together:
#' vpts1and2 <- bind_into_vpts(vpts1, vpts2)
#'
#' # verify that the binded vpts now has 20 profiles, 10 from vpts1 and 10 from
#' # vpts2:
#' summary(vpts1and2)
#'
#' # extract two profiles:
#' vp1 <- example_vpts[1]
#' vp1
#' vp2 <- example_vpts[2]
#' vp2
#'
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
  vplist_to_vpts(c.vp(...))
}

#' @describeIn bind_into_vpts Bind multiple \code{vp} objects into a
#' \code{vpts}. If data for multiple radars is provided, a list is returned
#' containing a \code{vpts} for each radar.
#'
#' @export
bind_into_vpts.list <- function(x, ...) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires list of vp objects as input")
  }
  vplist_to_vpts(x, ...)
}

#' @describeIn bind_into_vpts Bind multiple \code{vpts} into a single
#' \code{vpts}. Requires the input \code{vpts} to be from the same radar.
#'
#' @param attributes_from integer. Which \code{vpts} to copy attributes from (default:
#' first).
#'
#' @export
bind_into_vpts.vpts <- function(..., attributes_from = 1) {
  vptss <- list(...)
  vptstest <- sapply(vptss, function(x) is(x, "vpts"))
  if (FALSE %in% vptstest) {
    stop("requires vpts objects as input")
  }
  # extract radar identifiers
  radars <- unique(sapply(vptss, "[[", "radar"))
  if (length(radars) > 1) {
    stop("Vertical profiles are not from a single radar")
  }
  if (length(unique(lapply(vptss, "[[", "height"))) > 1) {
    stop("Vertical profiles have non-aligning altitude layers")
  }
  if (length(unique(lapply(vptss, function(x) names(x$"data")))) > 1) {
    stop("Vertical profiles have different quantities")
  }
  if (length(unique(sapply(vptss, function(x) x$attributes$where$interval)))>1){
    stop("Vertical profiles with different altitude layer widths")
  }
  if (length(unique(sapply(vptss, function(x) x$attributes$where$levels)))>1){
    stop("Vertical profiles with different numbers of altitude layers")
  }
  # extract date-times
  datetime <- .POSIXct(do.call("c", lapply(vptss, "[[", "datetime")), tz = "UTC")
  quantities <- names(vptss[[1]]$data)
  ordering <- order(datetime)
  datetime <- datetime[ordering]
  data <- lapply(
    quantities,
    function(quantity) {
      do.call(cbind, lapply(
        vptss,
        function(x) {
          x$"data"[[quantity]]
        }
      ))[, ordering]
    }
  )
  names(data) <- quantities
  difftimes <- difftime(datetime[-1], datetime[-length(datetime)], units = "secs")
  if (length(unique(difftimes)) == 1) {
    regular <- TRUE
  } else {
    regular <- FALSE
  }
  output <- list(
    radar = radars, datetime = datetime, height = vptss[[1]]$height,
    daterange = .POSIXct(c(min(datetime), max(datetime)), tz = "UTC"),
    timesteps = difftimes, data = data,
    attributes = vptss[[attributes_from]]$attributes,
    regular = regular
  )
  class(output) <- "vpts"
  output
}

#' Bind vertical profiles (\code{vp}) into time series (\code{vpts})
#'
#' Used as helper function for the method dispatched \code{bind_into_vpts} and
#' keeping backward compatibility with the \code{vpts} function.
#'
#' @param x A list of \code{vp} objects, usually a result of a call
#' to \link{read_vpfiles}.
#' @param radar optional string containing the radar identifier to generate
#' time series for.
#'
#' @return an object of class \link[=summary.vpts]{vpts} when \code{list}
#' contains profiles of a single radar. A list of objects of class
#' \link[=summary.vpts]{vpts} in case when \code{list} contains profiles of
#' multiple radars, containing \link[=summary.vpts]{vpts} objects for each radar.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' vps <- read_vpfiles(c("my/path/profile1.h5", "my/path/profile2.h5", ...))
#' ts <- bind_into_vpts(vps)
#' }
vplist_to_vpts <- function(x, radar = NA) {
  stopifnot(inherits(x, "list"))
  # extract radar identifiers
  radars <- sapply(x, "[[", "radar")
  uniqueRadars <- sort(unique(radars))
  if (!is.na(radar)) {
    if (!(radar %in% uniqueRadars)) {
      stop(paste("no profiles found for radar", radar))
    } else {
      return(vp_to_vpts_helper(x[which(radars == radar)]))
    }
  }
  if (is.na(radar) & (length(uniqueRadars) == 1)) {
    return(vp_to_vpts_helper(x[which(radars == uniqueRadars)]))
  } else {
    return(lapply(
      uniqueRadars,
      function(y) {
        vplist_to_vpts(x[radars == y])
      }
    ))
  }
}

vp_to_vpts_helper <- function(vps) {
  datetime <- .POSIXct(do.call("c", lapply(vps, "[[", "datetime")), tz = "UTC")
  daterange <- .POSIXct(c(min(datetime), max(datetime)), tz = "UTC")
  # sort by datetime
  vps <- vps[order(sapply(vps, "[[", "datetime"))]
  datetime <- .POSIXct(do.call("c", lapply(vps, "[[", "datetime")), tz = "UTC")
  difftimes <- difftime(datetime[-1], datetime[-length(datetime)], units = "secs")
  profile.quantities <- names(vps[[1]]$data)

  if (length(unique(lapply(vps, "[[", "height"))) > 1) {
    stop(paste(
      "Vertical profiles of radar", vps[[1]]$radar,
      "have non-aligning altitude layers."
    ))
  }
  if (length(unique(lapply(vps, function(x) names(x$"data")))) > 1) {
    stop(paste(
      "Vertical profiles of radar", vps[[1]]$radar,
      "contain different quantities."
    ))
  }
  if (length(unique(sapply(vps, function(x) x$attributes$where$interval)))>1){
    stop("Vertical profiles with different altitude layer widths")
  }
  if (length(unique(sapply(vps, function(x) x$attributes$where$levels)))>1){
    stop("Vertical profiles with different numbers of altitude layers")
  }

  vpsFlat <- lapply(
    profile.quantities,
    function(quantity) {
      sapply(lapply(vps, "[[", "data"), "[[", quantity)
    }
  )
  names(vpsFlat) <- profile.quantities
  if (length(unique(difftimes)) == 1) {
    regular <- TRUE
  } else {
    regular <- FALSE
  }
  vpsFlat$height <- NULL
  output <- list(
    radar = vps[[1]]$radar, datetime = datetime,
    height = vps[[1]]$data$height,
    daterange = .POSIXct(c(min(datetime), max(datetime)), tz = "UTC"),
    timesteps = difftimes, data = vpsFlat,
    attributes = vps[[1]]$attributes, regular = regular
  )
  class(output) <- "vpts"
  output
}
