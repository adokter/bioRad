#' Bind vertical profiles (`vp`) into time series (`vpts`)
#'
#' Binds vertical profiles (`vp`) into a vertical profile time series
#' (`vpts`), sorted in time. Can also bind multiple `vpts` of a
#' single radar into one `vpts`.
#'
#' @param x A `vp`, `vpts` or a vector of these.
#' @param ... A `vp`, `vpts` or a vector of these.
#'
#' @return A [`vpts()`][summary.vpts] for a single radar or a list of
#' `vpts` for multiple radars. Input `vp` are sorted in time in the
#' output `vpts`.
#'
#' @export
#'
#' @details `bind_into_vpts()` currently requires profiles to have aligning altitude
#' layers that are of equal width. Profiles are allowed to differ in the number
#' of altitude layers, i.e. the maximum altitude
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

#' @describeIn bind_into_vpts Bind multiple `vp` into a `vpts`.
#' If `vp` for multiple radars are provided, a list is returned containing
#' a `vpts` for each radar.
#'
#' @export
bind_into_vpts.vp <- function(...) {
  vps <- list(...)
  vptest <- sapply(vps, is.vp)
  if (FALSE %in% vptest) {
    stop("requires vp objects as input")
  }
  vplist_to_vpts(c.vp(...))
}

#' @describeIn bind_into_vpts Bind multiple `vp` objects into a
#' `vpts`. If data for multiple radars is provided, a list is returned
#' containing a `vpts` for each radar.
#'
#' @export
bind_into_vpts.list <- function(x, ...) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires list of vp objects as input")
  }
  vplist_to_vpts(x, ...)
}

#' @describeIn bind_into_vpts Bind multiple `vpts` into a single
#' `vpts`. Requires the input `vpts` to be from the same radar.
#'
#' @param attributes_from integer. Which `vpts` to copy attributes from (default:
#' first).
#'
#' @export
bind_into_vpts.vpts <- function(..., attributes_from = 1) {
  vptss <- list(...)
  vptstest <- sapply(vptss, is.vpts)
  if (FALSE %in% vptstest) {
    stop("requires vpts objects as input")
  }
  # extract radar identifiers
  radars <- unique(sapply(vptss, "[[", "radar"))
  if (length(radars) > 1) {
    stop("Vertical profiles are not from a single radar")
  }
  height_list <- lapply(vptss, "[[", "height")
  if (length(unique(height_list)) > 1) {
    target_heights <- combined_heights(height_list)
    vptss <- lapply(vptss, add_heights_vpts, target = target_heights)
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

#' Bind vertical profiles (`vp`) into time series (`vpts`)
#'
#' Used as helper function for the method dispatched `bind_into_vpts` and
#' keeping backward compatibility with the `vpts` function.
#'
#' @param x A list of `vp` objects, usually a result of a call
#' to [read_vpfiles].
#' @param radar optional string containing the radar identifier to generate
#' time series for.
#'
#' @return an object of class [vpts][summary.vpts] when `list`
#' contains profiles of a single radar. A list of objects of class
#' [vpts][summary.vpts] in case when `list` contains profiles of
#' multiple radars, containing [vpts][summary.vpts] objects for each radar.
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
combined_heights <- function(x) {
  assert_that(is.list(x))
  unique_height_diff <- unique(unlist(lapply(lapply(x, diff), unique)))
  if (length(unique_height_diff) != 1) {
    stop("Not all data has the same size of altitude bins")
  }
  height_alignment <- unique(unlist(x) %% unique_height_diff)
  if (length(height_alignment) != 1) {
    stop("Not all data has the same alignment of altitude bins")
  }
  return(sort(unique(unlist(x))))
}
add_heights_vpts <- function(x, target) {
  if (identical(x$height, target)) {
    return(x)
  }
  old <- target %in% x$height
  x$height <- target
  x$attributes$where$levels <- length(target)
  m <- matrix(nrow = length(target), ncol = length(x$datetime))
  x$data <- lapply(x$data, function(x, m, s) {
    m[s, ] <- x
    return(m)
  }, s = old, m = m)
  return(x)
}
add_heights_vp <- function(x, target) {
  if (identical(x$data$height, target)) {
    return(x)
  }
  x$data <- data.frame(rbindlist(list(x$data, data.frame(height = target[!(target %in% x$data$height)])), fill = TRUE))
  x$data <- x$data[order(x$data$height), ]
  x$attributes$where$levels <- length(target)
  return(x)
}
vp_to_vpts_helper <- function(vps) {
  datetime <- .POSIXct(do.call("c", lapply(vps, "[[", "datetime")), tz = "UTC")
  daterange <- .POSIXct(c(min(datetime), max(datetime)), tz = "UTC")
  # sort by datetime
  vps <- vps[order(sapply(vps, "[[", "datetime"))]
  datetime <- .POSIXct(do.call("c", lapply(vps, "[[", "datetime")), tz = "UTC")
  difftimes <- difftime(datetime[-1], datetime[-length(datetime)], units = "secs")
  profile.quantities <- names(vps[[1]]$data)
  height_list <- lapply(vps, function(x) x$data$height)
  if (length(unique(height_list)) > 1) {
    target_heights <- combined_heights(height_list)
    vps <- lapply(vps, add_heights_vp, target = target_heights)
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
