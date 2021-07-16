#' Inspect a time series of vertical profiles (`vpts`)
#'
#' R base functions for inspecting a time series of vertical profiles (`vpts`)
#' object.
#'
#' @param object A `vpts` object.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @method summary vpts
#'
#' @export
#'
#' @details
#' A time series of vertical profiles contains time-ordered vertical profiles
#' (`vp)` of a single radar. This time series can be **regular** (`vp` are
#' equally spaced in time) or **irregular** (time steps between `vp` are of
#' unequal length), indicated in the field `regular`. Irregular time series can
#' be projected onto a regular time grid with [regularize_vpts()]. A time series
#' of vertical profile (`vp`) object is a list containing:
#' * `radar`: Radar identifier.
#' * `datetime`: Nominal times of the profiles (named `dates` in bioRad <
#' 0.4.0) in UTC.
#' * `height`: Lowest height of the height bins in the profiles in m above sea
#' level.
#' * `daterange`: Minimum and maximum nominal time of the profiles in UTC.
#' * `timesteps`: Time differences between the profiles. Element `i` gives the
#' difference between profile `i` and `i+1`.
#' * `data`: A list of quantities, each containing a `datetime` by `height`
#' matrix with the values. Use [get_quantity()] to access these and see
#' [summary.vp()] for a description of available quantities.
#' * `attributes`: List of the vertical profile's `what`, `where`, and `how`
#' attributes, copied from the first profile.
#' * `regular`: Logical indicating whether the time series is regular or not.
#'
#' @section Conventions:
#' * `NA`: Maps to `nodata` in the ODIM convention: value to denote areas void
#' of data (never radiated).
#' * `NaN`: Maps to `undetect` in the ODIM convention: denote areas below the
#' measurement detection threshold (radiated but nothing detected). The value is
#' also used when there are too few datapoints to calculate a quantity.
#' * `0`: Maps to `0` in the ODIM convention: denote areas where the quantity
#' has a measured value of zero (radiated and value zero detected or inferred).
#'
#' @seealso
#' * [bind_into_vpts()]
#' * [read_vpts()]
#' * [filter_vpts()]
#' * [regularize_vpts()]
#' * [`example_vpts`]
#' * [get_quantity()]
#' * [plot.vp()]
#' * [as.data.frame.vpts()]
#' * \code{\link[=[.vpts]{[vpts()}}
#'
#' @examples
#' # Check if an object is of class vpts
#' is.vpts(example_vpts)
#'
#' # Get summary info
#' example_vpts # Same as summary(example_vpts) or print(example_vpts)
#'
#' # Get dimensions
#' dim(example_vpts)
summary.vpts <- function(object, ...) {
  print.vpts(object)
}

#' Print summary for an object of class `vpts`
#'
#' @noRd
#'
#' @export
print.vpts <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "vpts"))
  if (is.null(x[["height"]])) {
    warning(glue("`x` is a legacy `vpts` object without a column `height`. ",
            "Use convert_legacy() to avoid errors."))
    x <- convert_legacy(x)
  }
  if (is.null(x[["datetime"]])) {
    warning(glue("`x` is a legacy `vpts` object without a column `datetime`. ",
            "Use convert_legacy() to avoid errors."))
    x <- convert_legacy(x)
  }
  cat(
    "                  ",
    if (x$regular) {
      "Regular"
    } else {
      "Irregular"
    },
    "time series of vertical profiles (class vpts)\n\n"
  )
  cat("           radar: ", x$radar, "\n")
  cat("      # profiles: ", length(x$datetime), "\n")
  cat(
    "time range (UTC): ", format(x$daterange[1], "%Y-%m-%d %H:%M:%S"),
    "-", format(x$daterange[2], "%Y-%m-%d %H:%M:%S"), "\n"
  )
  if (length(x$timesteps) > 0) {
    stepMin <- min(x$timesteps)
    stepMax <- max(x$timesteps)
  } else {
    stepMin <- stepMax <- NA
  }
  if (x$regular & stepMin == stepMax) {
    cat("   time step (s): ", stepMin, "\n")
  } else {
    cat("   time step (s): ", "min:", stepMin, "    max: ", stepMax, "\n")
  }
}

#' Check if an object is of class `vpts`
#'
#' @param x A `vpts` object.
#'
#' @return For [is.vpts()]: `TRUE` for an object of class `vpts`, otherwise
#'   `FALSE`.
#'
#' @rdname summary.vpts
#'
#' @export
is.vpts <- function(x) {
  inherits(x, "vpts")
}

#' Get dimensions for an object of class `vpts`
#'
#' @return For [dim.vpts()]: number of datetimes, heights and quantities in a
#'   time series of vertical profiles (`vpts`).
#'
#' @rdname summary.vpts
#'
#' @export
dim.vpts <- function(x) {
  stopifnot(inherits(x, "vpts"))
  heights <- nrow(x$data[[1]])
  datetimes <- ncol(x$data[[1]])
  c(datetimes, heights, length(x$data))
}

#' Subset a time series of vertical profiles (`vpts`)
#'
#' Select a vertical profile (`vp`) or a time series of vertical profiles
#' (`vpts`) by index from a `vpts`.
#'
#' @param x A `vpts` object.
#' @param i Integer. Index/indices specifying which range of vertical profiles
#'   to extract.
#'
#' @return A `vpts` object containing a subset of vertical profiles (`vp`) or a
#'   `vp` object when subsetting a single vertical profile (`vp`).
#'
#' @export
#'
#' @examples
#' # The example vpts contains 1934 profiles (i.e. datetimes)
#' dim(example_vpts)
#'
#' # Subset vpts to extract 10th profile
#' example_vpts[10] # A vp object
#'
#' # Subset vpts to extract the 20th to 100th profile
#' example_vpts[20:100] # A vpts object with 81 profiles
#'
#' # Subset vpts to remove the first 10 profiles
#' example_vpts[-1:-10] # A vpts object with 10 less profiles
`[.vpts` <- function(x, i) {
  stopifnot(inherits(x, "vpts"))

  x$datetime <- x$datetime[i]
  x$daterange <- .POSIXct(c(min(x$datetime), max(x$datetime)), tz = "UTC")
  x$timesteps <- difftime(x$datetime[-1], x$datetime[-length(x$datetime)],
    units = "secs"
  )
  if (length(unique(x$timesteps)) == 1) {
    x$regular <- TRUE
  } else {
    x$regular <- FALSE
  }
  quantity.names <- names(x$data)
  x$data <- lapply(
    names(x$data),
    function(quantity) {
      getElement(x$data, quantity)[, i]
    }
  )
  names(x$data) <- quantity.names

  # Convert to vp if only 1 profile
  if(length(x$datetime) == 1) {
    x <- vpts_to_vp(x)
  }

  return(x)
}

#' Helper function to convert a vpts[1] to a vp object
#'
#' @noRd
vpts_to_vp <- function(x) {
  stopifnot(inherits(x, "vpts"))
  stopifnot(length(x$datetime) == 1)

  vpout <- list()
  vpout$radar <- x$radar
  vpout$datetime <- x$datetime[1]
  vpout$data <- as.data.frame(lapply(
    names(x$data),
    function(y) {
      x$data[y][[1]]
    }
  ))
  names(vpout$data) <- names(x$data)
  vpout$attributes <- x$attributes
  vpout$data$height <- x$height
  class(vpout) <- "vp"
  vpout
}
