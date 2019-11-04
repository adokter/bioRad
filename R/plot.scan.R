#' Plot a scan (\code{scan}) in polar coordinates
#'
#' Plots a scan in polar coordinates. For plots in Cartesian coordinates,
#' see \link{ppi}
#'
#' @param x An object of class \code{scan}.
#' @param param The scan parameter to plot, see details below.
#' @param xlim Range of x (range, distance from radar) values to plot.
#' @param ylim Range of y (azimuth) values to plot.
#' @param zlim The range of parameter values to plot.
#' @param na.value \link[ggplot2]{ggplot} argument setting the plot color of NA values
#' @param ... Arguments passed to low level \link[ggplot2]{ggplot} function.
#'
#' @method plot scan
#'
#' @export
#' @details
#' Available scan parameters for plotting can by printed to screen
#' by \code{summary(x)}. Commonly available parameters are:
#' \describe{
#'  \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'  \item{"\code{TH}", "\code{T}"}{(Logged) uncorrected reflectivity factor [dBZ]}
#'  \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity [m/s]. Radial
#'    velocities towards the radar are negative, while radial velocities away
#'    from the radar are positive}
#'  \item{"\code{RHOHV}"}{Correlation coefficient [unitless]. Correlation
#'    between vertically polarized and horizontally polarized
#'    reflectivity factor}
#'  \item{"\code{PHIDP}"}{Differential phase [degrees]}
#'  \item{"\code{ZDR}"}{(Logged) differential reflectivity [dB]}
#' }
#' The scan parameters are named according to the OPERA data information
#' model (ODIM), see Table 16 in the
#' \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
#'
#' @examples
#' # load an example scan:
#' data(example_scan)
#' # print to screen the available scan parameters
#' summary(example_scan)
#' # make ppi for the scan
#' # plot the reflectivity param:
#' plot(example_scan, param = "DBZH")
#' # change the range of reflectivities to plot to -30 to 50 dBZ:
#' plot(example_scan, param = "DBZH", zlim = c(-30, 50))
plot.scan <- function(x, param, xlim = c(0, 100000),
                      ylim = c(0, 360), zlim = c(-20, 20), na.value = "transparent", ...) {
  stopifnot(inherits(x, "scan"))

  if(hasArg("quantity")) stop("unknown function argument 'quantity`. Did you mean `param`?")

  if (missing(param)) {
    if ("DBZH" %in% names(x$data)) {
      param <- "DBZH"
    } else {
      param <- names(x$params)[1]
    }
  } else if (!is.character(param)) {
    stop(
      "'param' should be a character string with a valid scan",
      "parameter name"
    )
  }
  if (missing(zlim)) {
    zlim <- get_zlim(param, zlim)
  }
  colorscale <- color_scale_fill(param, zlim, na.value)
  # extract the scan parameter
  y <- NULL # dummy asignment to suppress devtools check warning
  data <- do.call(function(y) x$params[[y]], list(param))
  # remove the param class label, to enable raster function
  class(data) <- "matrix"
  # convert to points
  dimraster <- dim(data)
  ascale=c(x$attributes$where$nrays) / 360
  rscale=c(x$attributes$where$rscale)
  data <- raster::as.data.frame(raster(t(data), ymn=0,ymx=360, xmn=0, xmx=rscale*dimraster[1]),xy=T)
  # change the name from "layer" to the parameter names
  names(data) <- c("range", "azimuth", param)

  # bring z-values within plotting range
  index <- which(data[, 3] < zlim[1])
  if (length(index) > 0) {
    data[index, 3] <- zlim[1]
  }
  index <- which(data[, 3] > zlim[2])
  if (length(index) > 0) {
    data[index, 3] <- zlim[2]
  }
  # plot
  azimuth <- NULL # dummy asignment to suppress devtools check warning
  bbox <- coord_cartesian(xlim = xlim, ylim = ylim)
  ggplot(data = data, ...) +
    geom_raster(aes(x = range, y = azimuth, fill = eval(parse(text = param)))) +
    colorscale +
    bbox
}
