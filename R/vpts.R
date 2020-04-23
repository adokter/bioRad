#' Inspect a time series of vertical profiles (`vpts`)
#'
#' R base functions for inspecting a time series of vertical profiles (`vp`)
#' object.
#'
#' @param object A `vpts` object.
#' @param x A `vpts` object.
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
#' * `datetime`: Nominal times of the profiles (named `dates` in biorad <
#' 0.4.0) in UTC.
#' * `height`: Lowest height of the height bins in the profiles in m.
#' * `daterange`: Minimum and maximum nominal time of the profiles in UTC.
#' * `timesteps`: Time differences between the profiles. Element `i` gives the
#' difference between profile `i` and `i+1`.
#' * `data`: A list of quantities, each containing a `datetime` by `height`
#' matrix with the values. Use [get_quantity()] to access these and see
#' [summary.vp()] for a description of available quantities.
#' * `attributes`: List of the vertical profile's `where` and `how` attributes,
#' copied from the first profile.
#' * `regular`: Logical indicating whether the time series is regular or not.
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
#' # Load the example time series of vertical profiles
#' vpts <- example_vpts
#'
#' # Verify that it is an object of class vpts
#' is.vpts(vpts)
#'
#' # Get summary info
#' vpts # Same as summary(vpts) or print(vpts)
#'
#' # Get dimensions
#' dim(vpts)
summary.vpts <- function(object, ...) {
  print.vpts(object)
}

#' Print summary for an object of class `vpts`
#'
#' @inheritParams summary.vpts
#'
#' @rdname summary.vpts
#'
#' @keywords internal
#'
#' @export
print.vpts <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  stopifnot(inherits(x, "vpts"))
  # check if we are dealing with a deprecated vpts class structure
  if (!is.null(x$heights)) {
    warning("obsolete vtps object generated with bioRad version < 0.5.0.
    vpts objects should contain a list element 'height' (instead of obsolete 'heights')")
  }
  if (!is.null(x$dates)) {
    warning("obsolete vtps object generated with bioRad version < 0.4.0.
    vpts objects should contain a list element 'datetime' (instead of obsolete 'dates')")
    x$datetime <- x$dates
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

#' Verify if an object is of class `vpts`
#'
#' @inheritParams summary.vpts
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
#' @return For [dim.vpts()]: number of heights, datetimes and quantities in a
#'   time series of vertical profiles (`vpts`).
#'
#' @rdname summary.vpts
#'
#' @export
dim.vpts <- function(x) {
  stopifnot(inherits(x, "vpts"))
  data.dim <- dim(x$data[[1]])
  c(data.dim, length(x$data))
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
#' # Load the example time series of vertical profiles
#' vpts <- example_vpts
#'
#' # This vpts contains 1934 profiles (i.e. datetimes)
#' dim(vpts)
#'
#' # Subset vpts to extract 10th profile (returns a vp object)
#' vpts[10]
#'
#' # Subset vpts to extract the 20th to 100th profile (returns a vpts object)
#' vpts[20:100]
`[.vpts` <- function(x, i) {
  stopifnot(inherits(x, "vpts"))
  if (length(i) < 1) {
    stop("Time series should contain more than one profile.")
  }
  if (length(i) == 1) {
    if (i > 0) {
      return(vpts_to_vp(x, i))
    } else {
      if (dim(x)[2] == 2) {
        if (i == -1) {
          return(vpts_to_vp(x, 2))
        }
        if (i == -2) {
          return(vpts_to_vp(x, 1))
        }
      }
    }
  }
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
  return(x)
}

#' Convert a time series of vertical profiles (\code{vpts}) to a data frame
#'
#' Converts vertical profile time series (objects of class \code{vpts}) to a
#' data Frame, and optionally adds information on sunrise/sunset, day/night
#' and derived quantities like migration traffic rates.
#'
#' @param x An object of class \code{vpts}.
#' @param row.names \code{NULL} or a character vector giving the row names for
#' the data frame. Missing values are not allowed.
#' @param optional If \code{FALSE} then the names of the variables in the data
#' frame are checked to ensure that they are syntactically valid variable names
#' and are not duplicated.
#' @param quantities An optional character vector with the names of the
#' quantities to include as columns in the data frame.
#' @param elev Sun elevation in degrees, see \link{sunrise}/\link{sunset}.
#' @param lat Radar latitude in decimal degrees. When set, overrides the
#' latitude stored in \code{x} in \link{sunrise}/\link{sunset} calculations.
#' @param lon Radar longitude in decimal degrees. When set, overrides the
#' longitude stored in \code{x} in \link{sunrise}/\link{sunset} calculations.
#' @param suntime Logical, when TRUE, adds sunrise/sunset and day/night
#' information to each row.
#' @param geo Logical, when TRUE, adds latitude, longitude and antenna height
#' of the radar to each row.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return An object of class data.frame.
#'
#' @export
#'
#' @details
#' Note that only the 'dens' quantity is thresholded for radial velocity
#' standard deviation by \link{sd_vvp_threshold}. Note that this is different from the
#' default \link{plot.vp}, \link{plot.vpts} and \link{get_quantity.vp}
#' functions, where quantities "eta", "dbz", "ff", "u", "v", "w", "dd" are all
#' thresholded by \link{sd_vvp_threshold}.
#'
#' @examples
#' # Load the example time series of vertical profiles
#' vpts <- example_vpts
#'
#' # Convert to a data.frame
#' vpts_df <- as.data.frame(vpts)
#'
#' # Print the first 10 rows of the data.frame
#' vpts_df[1:10, ]
#'
#' # Do not compute sunrise/sunset information
#' vpts_df <- as.data.frame(vpts, suntime = FALSE)
#'
#' # Override the latitude/longitude information stored in the object when
#' # calculating sunrise/sunset information
#' vpts_df <- as.data.frame(vpts, suntime = TRUE, lat = 50, lon = 4)
as.data.frame.vpts <- function(x, row.names = NULL, optional = FALSE,
                               quantities = names(x$data), suntime = TRUE,
                               geo = TRUE, elev = -0.268, lat = NULL,
                               lon = NULL, ...) {
  stopifnot(inherits(x, "vpts"))
  if (!is.null(row.names)) {
    if (is.character(row.names) & length(row.names) ==
      length(x$datetime) * length(x$height)) {
      rownames(output) <- row.names
    } else {
      stop(paste(
        "'row.names' is not a character vector of length",
        length(x$datetime) * length(x$height)
      ))
    }
  }
  if (is.null(lat)) {
    lat <- x$attributes$where$lat
  }
  if (is.null(lon)) {
    lon <- x$attributes$where$lon
  }
  missing <- which(!(quantities %in% names(x$data)))
  if (length(missing) > 0) {
    stop(paste(
      paste(quantities[missing], collapse = " "),
      "not an available quantity, select one or more of",
      paste(names(x$data), collapse = ",")
    ))
  }
  # coerce data to a data frame
  output <- as.data.frame(lapply(x$data[quantities], c),
    optional = optional, ...
  )
  # add height and datetime as a column
  output <- cbind(
    datetime = as.POSIXct(
      c(t(replicate(length(x$height), x$datetime))),
      origin = "1970-1-1", tz = "UTC"
    ),
    height = rep(x$height, length(x$datetime)), output
  )
  # add radar name
  output <- cbind(radar = x$radar, output, stringsAsFactors = FALSE)
  # add location information
  if (geo) {
    output$lat <- lat
    output$lon <- lon
    output$height_antenna <- x$attributes$where$height
  }
  # override the lat,lon attributes in case of user-provided values
  x$attributes$where$lat <- lat
  x$attributes$where$lon <- lon
  # add day
  if (suntime) {
    dayQ <- !check_night(x, elev = elev)
    dayQ <- c(t(replicate(length(x$height), dayQ)))
    output <- cbind(output, day = dayQ)
    sunrise <- sunrise(x$datetime, lat = lat, lon = lon)
    sunset <- sunset(x$datetime, lat = lat, lon = lon)
    output$sunrise <- as.POSIXct(
      c(t(replicate(length(x$height), sunrise))),
      origin = "1970-1-1", tz = "UTC"
    )
    output$sunset <- as.POSIXct(
      c(t(replicate(length(x$height), sunset))),
      origin = "1970-1-1", tz = "UTC"
    )
  }
  output
}

vpts_to_vp <- function(x, i) {
  stopifnot(inherits(x, "vpts"))
  nvp <- dim(x)[2]
  if (i < 1 || i > nvp) {
    return(NA)
  }
  vpout <- list()
  vpout$radar <- x$radar
  vpout$datetime <- x$datetime[i]
  vpout$data <- as.data.frame(lapply(
    names(x$data),
    function(y) {
      x$data[y][[1]][, i]
    }
  ))
  names(vpout$data) <- names(x$data)
  vpout$attributes <- x$attributes
  vpout$data$height <- x$height
  class(vpout) <- "vp"
  vpout
}
