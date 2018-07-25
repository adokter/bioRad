#' Plot an integrated profile (\code{vpi})
#'
#' Plot an object of class \code{vpi}.
#'
#' @param x 1 class object inheriting from class \code{vpi}, typically a
#' call to \link[bioRad]{integrate_profile}.
#' @param quantity Character string with the quantity to plot, one of
#' '\code{vid}','\code{vir}','\code{mtr}' for vertically integrated density,
#' reflectivity, reflectivity factor and migration traffic rate, respectively.
#' @param ylim y-axis plot range, numeric atomic vector of length 2.
#' @param xlab A title for the x-axis.
#' @param ylab A title for the y-axis.
#' @param main A title for the plot.
#' @param nightshade Logical, whether to plot night time shading.
#' @param elev Numeric, sun elevation to use for day/night transition,
#' see \link[bioRad]{suntime}.
#' @param lat (optional) Latitude in decimal degrees. Overrides the lat
#' attribute of \code{x}.
#' @param lon (optional) Longitude in decimal degrees. Overrides the lon
#' attribute of \code{x}.
#' @param ... Additional arguments to be passed to the low level
#' \link[graphics]{plot} plotting function.
#'
#' @method plot vpi
#'
#' @export
#'
#' @details
#' Profile can be visualised in three related quantities, as specified by
#' argument \code{quantity}:
#' \describe{
#'  \item{"\code{vid}"}{Vertically Integrated Density, i.e. the aerial surface
#'    density of individuals. This quantity is dependent on the assumed radar
#'    cross section per individual (RCS)}
#'  \item{"\code{vir}"}{Vertically Integrated Reflectivity. This quantity is
#'    independent of the value of individual's radar cross section}
#'  \item{"\code{mtr}"}{Migration Traffic Rate. This quantity is dependent on
#'    the assumed radar cross section (RCS)}
#'  \item{"\code{rtr}"}{Reflectivity Traffic Rate. This quantity is independent
#'    on the assumed radar cross section (RCS)}
#' }
#'
#' @examples
#' # vertically integrate a vpts object:
#' vpi <- integrate_profile(example_vpts)
#' # plot the migration traffic rates
#' plot(vpi)
#' # plot the vertically integrated densities, without night shading:
#' plot(vpi, quantity = "vid", nightshade = FALSE)
plot.vpi <- function(x, quantity = "mtr", xlab = "time",
                     ylab = "migration traffic rate [#/km/h]",
                     main = "MTR", nightshade = TRUE, elev = -0.268,
                     lat = NULL, lon = NULL, ylim = NULL, ...) {
  stopifnot(inherits(x, "vpi"))
  stopifnot(quantity %in% c("mtr", "vid", "vir", "rtr", "mt",
                            "rt", "ff", "dd", "u", "v"))

  # set up the plot labels
  if (missing(ylab)) {
    if (quantity == "mtr") ylab = "migration traffic rate [#/km/h]"
    if (quantity == "rtr") ylab = expression("reflectivity traffic rate [cm"^2*"/km/h]")
    if (quantity == "vid") ylab = expression("vertically integrated density [#/km"^2*"]")
    if (quantity == "vir") ylab = expression("vertically integrated reflectivity [cm"^2*"/km/h]")
    if (quantity == "mt") ylab = expression("(cumulative) migration traffic [#/km]")
    if (quantity == "rt") ylab = expression("(cumulative) reflectivity traffic [cm"^2*"/km]")
    if (quantity == "ff") ylab = expression("vertically averaged ground speed [m/s]")
    if (quantity == "dd") ylab = expression("vertically averaged direction [deg]")
    if (quantity == "u") ylab = expression("vertically averaged u-component ground speed [m/s]")
    if (quantity == "v") ylab = expression("vertically averaged v-component ground speed [m/s]")
  }
  if (missing(main)) {
    if (quantity == "mtr") main = "MTR"
    if (quantity == "rtr") main = "RTR"
    if (quantity == "vid") main = "VID"
    if (quantity == "vir") main = "VIR"
    if (quantity == "mt") main = "MT"
    if (quantity == "rt") main = "RT"
    if (quantity == "ff") main = "Ground speed"
    if (quantity == "dd") main = "Ground speed direction"
    if (quantity == "u") main = "Ground speed u (east->west)"
    if (quantity == "v") main = "Ground speed v (north->south)"
  }
  if (missing(lat)) lat = attributes(x)$lat
  if (missing(lon)) lon = attributes(x)$lon
  
  # plot the data
  plot(x$datetime, x[quantity][[1]], type = 'l', xlab = "time", ylab = ylab,
       ylim = ylim, main = main, xaxs = "i", yaxs = "i", ...)

  if (nightshade) {
    if (!is.numeric(lat) || !is.numeric(lon)) {
      stop("No latitude/longitude found in attribute data, please provide",
           "lat and lon arguments when nightshade=TRUE.")
    }

    # calculate sunrise and sunset
    days <- as.POSIXct(seq(as.Date(min(x$datetime) - 24*3600),
                           as.Date(max(x$datetime) + 24*3600),
                           by = "days"), tz = "UTC")
    trise <- sunrise(lon, lat, days)
    tset <- sunset(lon, lat, days)
    
    if (trise[1] < tset[1]) {
      trise = trise[-1]
      tset = tset[-length(tset)]
    }

    # determine the plot range of the night time shading
    if (missing(ylim)) {
      pol.range <- c(min(c(0, 2*min(x[quantity][[1]]))),
                    2*max(x[quantity][[1]]))
    } else {
      pol.range <- ylim
    }
    ypolygon <- c(pol.range[1], pol.range[1],
                  pol.range[2], pol.range[2])

    # plot night time shading for each night.
    for (i in 1:length(days)) {
      polygon(c(tset[i], trise[i], trise[i], tset[i]),
              ypolygon, lty = 0, col = "#CCCCCC")
    }

    # plot the data again on top of the shading
    points(x$datetime, x[quantity][[1]], type = 'l',
           xlab = "time", ylab = ylab, ylim = ylim, main = main, ...)
  }
}
