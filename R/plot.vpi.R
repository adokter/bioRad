#' Plot an integrated profile (`vpi`)
#'
#' Plot an object of class `vpi`.
#'
#' @param x 1 class object inheriting from class `vpi`, typically a
#' call to [integrate_profile][bioRad::integrate_profile].
#' @param quantity Character string with the quantity to plot, one of
#' * `vid` (vertically integrated density),
#' * `vir` (vertically integrated reflectivity),
#' * `mtr` (migration traffic rate),
#' * `rtr` (reflectivity traffic rate),
#' * `mt` ((cumulative) migration traffic),
#' * `rt` ((cumulative) reflectivity traffic),
#' * `ff` (height-averaged ground speed)
#' * `dd` (height-averaged direction)
#' * `u` (height-averaged u-component of ground speed),
#' * `v` (height-averaged v-component of ground speed).
#' @param ylim y-axis plot range, numeric atomic vector of length 2.
#' @param xlab A title for the x-axis.
#' @param ylab A title for the y-axis.
#' @param main A title for the plot.
#' @param night_shade Logical, whether to plot night time shading.
#' @param elev Numeric, sun elevation to use for day/night transition,
#' see [sunrise].
#' @param lat (optional) Latitude in decimal degrees. Overrides the lat
#' attribute of `x`.
#' @param lon (optional) Longitude in decimal degrees. Overrides the lon
#' attribute of `x`.
#' @param ... Additional arguments to be passed to the low level
#' [plot][graphics::plot] plotting function.
#' @param nightshade Deprecated argument, use night_shade instead.
#'
#' @method plot vpi
#'
#' @export
#'
#' @details
#' The integrated profiles can be visualized in various related quantities, as specified by
#' argument `quantity`:
#' * `vid`: Vertically Integrated Density, i.e. the aerial surface density of
#'   individuals. This quantity is dependent on the assumed radar cross section
#'   per individual (RCS)
#' * `vir`: Vertically Integrated Reflectivity. This quantity is independent of
#'   the value of individual's radar cross section
#' * `mtr`: Migration Traffic Rate. This quantity is dependent on the assumed
#'   radar cross section (RCS)
#' * `rtr`: Reflectivity Traffic Rate. This quantity is independent on the
#'   assumed radar cross section (RCS)
#' * `mt`: Migration Traffic. This quantity is dependent on the assumed radar
#'   cross section (RCS)
#' * `rt`: Reflectivity Traffic. This quantity is independent on the assumed
#'   radar cross section (RCS)
#' * `ff`: Horizontal ground speed in m/s
#' * `dd`: Horizontal ground speed direction in degrees
#' * `u`: Ground speed component west to east in m/s
#' * `v`: Ground speed component south to north in m/s
#' * `height`: Mean flight height (height weighted by reflectivity eta) in m
#'   above sea level
#' The height-averaged ground speed quantities (ff,dd,u,v) and height are weighted averages by reflectivity eta.
#' @examples
#' # vertically integrate a vpts object:
#' vpi <- integrate_profile(example_vpts)
#' # plot the migration traffic rates
#' plot(vpi)
#' # plot the vertically integrated densities, without night shading:
#' plot(vpi, quantity = "vid", night_shade = FALSE)
plot.vpi <- function(x, quantity = "mtr", xlab = "time",
                     ylab = "migration traffic rate [#/km/h]",
                     main = "MTR", night_shade = TRUE,
                     elev = -0.268, lat = NULL, lon = NULL, ylim = NULL, nightshade = TRUE, ...) {
  stopifnot(inherits(x, "vpi"))
  assert_that(
    quantity %in% names(x) & quantity != "datetime",
    msg = glue("quantity `{quantity}` not found in vpi object.")
  )

  if (hasArg("param")) stop("unknown function argument 'param`. Did you mean `quantity`?")

  # deprecate function argument
  if (!missing(nightshade)) {
    warning("argument nightshade is deprecated; please use night_shade instead.",
      call. = FALSE
    )
    night_shade <- nightshade
  }

  # set up the plot labels
  if (missing(ylab)) {
    if (quantity == "mtr") ylab <- "migration traffic rate [#/km/h]"
    if (quantity == "rtr") ylab <- expression("reflectivity traffic rate [cm"^2 * "/km/h]")
    if (quantity == "vid") ylab <- expression("vertically integrated density [#/km"^2 * "]")
    if (quantity == "vir") ylab <- expression("vertically integrated reflectivity [cm"^2 * "/km/h]")
    if (quantity == "mt") ylab <- expression("(cumulative) migration traffic [#/km]")
    if (quantity == "rt") ylab <- expression("(cumulative) reflectivity traffic [cm"^2 * "/km]")
    if (quantity == "ff") ylab <- expression("vertically averaged ground speed [m/s]")
    if (quantity == "dd") ylab <- expression("vertically averaged direction [deg]")
    if (quantity == "u") ylab <- expression("vertically averaged u-component ground speed [m/s]")
    if (quantity == "v") ylab <- expression("vertically averaged v-component ground speed [m/s]")
    if (quantity == "height") ylab <- expression("vertically averaged flight height above sea level [m]")
  }
  if (missing(main)) {
    if (quantity == "mtr") main <- "MTR"
    if (quantity == "rtr") main <- "RTR"
    if (quantity == "vid") main <- "VID"
    if (quantity == "vir") main <- "VIR"
    if (quantity == "mt") main <- "MT"
    if (quantity == "rt") main <- "RT"
    if (quantity == "ff") main <- "Average ground speed"
    if (quantity == "dd") main <- "Average ground speed direction"
    if (quantity == "u") main <- "Average ground speed u (east->west)"
    if (quantity == "v") main <- "Average ground speed v (north->south)"
    if (quantity == "height") main <- "Average flight height above sea level"
  }
  if (missing(lat)) lat <- attributes(x)$lat
  if (missing(lon)) lon <- attributes(x)$lon

  # plot the data
  plot(x$datetime, x[quantity][[1]],
    type = "l", xlab = "time", ylab = ylab,
    ylim = ylim, main = main, xaxs = "i", yaxs = "i", ...
  )

  if (night_shade) {
    if (!is.numeric(lat) || !is.numeric(lon)) {
      stop(
        "No latitude/longitude found in attribute data, please provide",
        "lat and lon arguments when night_shade=TRUE."
      )
    }

    # calculate sunrise and sunset
    days <- as.POSIXct(seq(as.Date(min(x$datetime) - 24 * 3600),
      as.Date(max(x$datetime) + 24 * 3600),
      by = "days"
    ), tz = "UTC")

    trise <- sunrise(days, lon, lat, elev = elev)
    tset <- sunset(days, lon, lat, elev = elev)

    if (trise[1] < tset[1]) {
      trise <- trise[-1]
      tset <- tset[-length(tset)]
    }

    # determine the plot range of the night time shading
    if (missing(ylim)) {
      pol.range <- c(
        min(c(0, 2 * min(x[quantity][[1]]))),
        2 * max(x[quantity][[1]])
      )
    } else {
      pol.range <- ylim
    }
    ypolygon <- c(
      pol.range[1], pol.range[1],
      pol.range[2], pol.range[2]
    )

    # plot night time shading for each night.
    for (i in 1:length(days)) {
      polygon(c(tset[i], trise[i], trise[i], tset[i]),
        ypolygon,
        lty = 0, col = "#CCCCCC"
      )
    }

    # plot the data again on top of the shading
    points(x$datetime, x[quantity][[1]],
      type = "l",
      xlab = "time", ylab = ylab, ylim = ylim, main = main, ...
    )
  }
}
