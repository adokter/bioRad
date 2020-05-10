#' Plot a plan position indicator (\code{ppi})
#'
#' Plot a plan position indicator (PPI) generated with \link{ppi}
#' using \link[ggplot2]{ggplot}
#'
#' @param x An object of class \code{ppi}.
#' @param param The scan parameter to plot, see details below.
#' @param xlim Range of x values to plot.
#' @param ylim Range of y values to plot.
#' @param ratio Aspect ratio between x and y scale.
#' @param zlim The range of parameter values to plot.
#' @param na.value \link[ggplot2]{ggplot} argument setting the plot color of NA values
#' @param ... Arguments passed to low level \link[ggplot2]{ggplot} function.
#'
#' @method plot ppi
#'
#' @export
#'
#' @details
#' Available scan parameters for plotting can by printed to screen
#' by \code{summary(x)}. Commonly available parameters are:
#' \describe{
#'  \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor (dBZ)}
#'  \item{"\code{TH}", "\code{T}"}{(Logged) uncorrected reflectivity factor (dBZ)}
#'  \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity (m/s). Radial
#'  velocities towards the radar are negative, while radial velocities away
#'  from the radar are positive}
#'  \item{"\code{RHOHV}"}{Correlation coefficient (unitless). Correlation
#'  between vertically polarized and horizontally polarized reflectivity factor}
#'  \item{"\code{PHIDP}"}{Differential phase (degrees)}
#'  \item{"\code{ZDR}"}{(Logged) differential reflectivity (dB)}
#' }
#' The scan parameters are named according to the OPERA data information
#' model (ODIM), see Table 16 in the
#' \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
#'
#' @examples
#' # load an example scan:
#' data(example_scan)
#'
#' # print to screen the available scan parameters:
#' summary(example_scan)
#'
#' # make ppi for the scan
#' ppi <- project_as_ppi(example_scan)
#'
#' # plot the first scan parameter, which in this case is "VRADH":
#' plot(ppi)
#'
#' # plot the reflectivity parameter:
#' plot(ppi, param = "DBZH")
#'
#' # change the range of reflectivities to plot, from -10 to 10 dBZ:
#' plot(ppi, param = "DBZH", zlim = c(-10, 10))
#'
#' # change the scale name and colour scheme, using viridis colors:
#' plot(ppi, param = "DBZH", zlim = c(-10, 10)) + viridis::scale_fill_viridis(name = "dBZ")
plot.ppi <- function(x, param, xlim, ylim, zlim = c(-20, 20),
                     ratio = 1, na.value = "transparent", ...) {
  stopifnot(inherits(x, "ppi"))

  if (hasArg("quantity")) stop("unknown function argument 'quantity`. Did you mean `param`?")

  if (missing(param)) {
    if ("DBZH" %in% names(x$data)) {
      param <- "DBZH"
    } else {
      param <- names(x$data)[1]
    }
  } else if (!see_if(param %in% names(x$data))) {
    stop(paste("parameter '", param, "' not found in PPI", sep = ""))
  }

  if (missing(zlim)) {
    zlim <- get_zlim(param, zlim)
  }
  colorscale <- color_scale_fill(param, zlim, na.value)
  # extract the scan parameter
  y <- NULL # dummy asignment to suppress devtools check warning
  data <- do.call(function(y) x$data[y], list(param))
  # convert to points
  data <- raster::as.data.frame(raster(data), xy = T)
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
  if (missing(xlim)) {
    xlim <- x$data@bbox[1, ]
  }
  if (missing(ylim)) {
    ylim <- x$data@bbox[2, ]
  }
  bbox <- coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)
  ggplot(data = data, ...) +
    geom_raster(aes(x, y, fill = eval(parse(text = param)))) +
    colorscale +
    bbox
}
