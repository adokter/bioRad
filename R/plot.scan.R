#' Plot a scan (\code{scan}) in polar coordinates
#'
#' Plots a scan in polar coordinates. For plots in Cartesian coordinates,
#' see \link{ppi}
#'
#' @param x An object of class \code{scan}.
#' @param quantity The scan parameter to plot, see details below.
#' @param xlim Range of x (range, distance from radar) values to plot.
#' @param ylim Range of y (azimuth) values to plot.
#' @param zlim The range of parameter values to plot.
#' @param ... Arguments passed to low level \link[ggplot2]{ggplot} function.
#' @param param Deprecated argument, use quantity instead.
#'
#' @method plot scan
#'
#' @export
#' @details
#' Available scan quantities for plotting can by printed to screen
#' by \code{summary(x)}. Commonly available quantities are:
#' \describe{
#'  \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'  \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity [m/s]. Radial
#'    velocities towards the radar are negative, while radial velocities away
#'    from the radar are positive}
#'  \item{"\code{RHOHV}"}{Correlation coefficient [unitless]. Correlation
#'    between vertically polarized and horizontally polarized
#'    reflectivity factor}
#'  \item{"\code{PHIDP}"}{Differential phase [degrees]}
#'  \item{"\code{ZDR}"}{(Logged) differential reflectivity [dB]}
#' }
#' The scan quantities are named according to the OPERA data information
#' model (ODIM), see Table 16 in the
#' \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
#'
#' @examples
#' # load an example scan:
#' data(example_scan)
#' # print to screen the available scan quantities
#' summary(example_scan)
#' # make ppi for the scan
#' # plot the reflectivity quantity:
#' plot(example_scan, quantity = "DBZH")
#' # change the range of reflectivities to plot to -30 to 50 dBZ:
#' plot(example_scan, quantity = "DBZH", zlim = c(-30, 50))
plot.scan <- function(x, quantity, param, xlim = c(0, 100),
                      ylim = c(0, 360), zlim = c(-20, 20), ...) {
  stopifnot(inherits(x, "scan"))

  # deprecate function argument
  if (!missing(param)) {
    warning("argument param is deprecated; please use quantity instead.",
            call. = FALSE)
    quantity <- param
  }

  if (missing(quantity)) {
    if ("DBZH" %in% names(x$data)) {
      quantity <- "DBZH"
    } else {
      quantity <- names(x$params)[1]
    }
  } else if (!is.character(quantity)) {
    stop("'quantity' should be a character string with a valid scan",
         "quantity name")
  }
  if (missing(zlim)) {
    zlim <- get_zlim(quantity)
  }
  colorscale <- color_scale_fill(quantity, zlim)
  # extract the scan quantity
  y <- NULL #dummy asignment to suppress devtools check warning
  data <- do.call(function(y) x$params[[y]], list(quantity))
  # remove the param class label, to enable raster function
  class(data) <- "matrix"
  # convert to points
  dimraster <- dim(data)
  data <- data.frame(rasterToPoints(raster(data)))
  data$x <- (1 - data$x) * dimraster[2] * x$attributes$where$nrays / 360
  data$y <- (1 - data$y) * dimraster[1] * x$attributes$where$rscale / 1000
  # change the name from "layer" to the quantity names
  names(data) <- c("azimuth", "range", quantity)
  # bring z-values within plotting range
  index <- which(data[,3] < zlim[1])
  if (length(index) > 0) {
    data[index,3] <- zlim[1]
  }
  index <- which(data[,3] > zlim[2])
  if (length(index) > 0) {
    data[index,3] <- zlim[2]
  }
  # plot
  azimuth <- NULL #dummy asignment to suppress devtools check warning
  ggplot(data = data,...) +
    geom_raster(aes(x = range, y = azimuth, fill = eval(parse(text = quantity)))) +
    colorscale +
    xlim(xlim[1], xlim[2]) +
    ylim(ylim[1], ylim[2])
}
