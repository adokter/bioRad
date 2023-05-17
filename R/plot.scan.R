#' Plot a scan (`scan`) in polar coordinates
#'
#' Plots a scan in polar coordinates. For plots in Cartesian coordinates,
#' see `project_to_ppi`
#'
#' @param x An object of class `scan`.
#' @param param The scan parameter to plot, see details below.
#' @param xlim Range of x (range, distance from radar) values to plot.
#' @param ylim Range of y (azimuth) values to plot.
#' @param zlim The range of parameter values to plot.
#' @param na.value [ggplot][ggplot2::ggplot] argument setting the plot color of NA values
#' @param ... Arguments passed to low level [ggplot][ggplot2::ggplot] function.
#'
#' @method plot scan
#'
#' @export
#' @details
#' Available scan parameters for plotting can by printed to screen
#' by `summary(x)`. Commonly available parameters are:
#' - `DBZH`, `DBZ`: (Logged) reflectivity factor (dBZ)
#' - `TH`, `T`: (Logged) uncorrected reflectivity factor (dBZ)
#' - `VRADH`, `VRAD`: Radial velocity (m/s). Radial velocities towards the radar
#'   are negative, while radial velocities away from the radar are positive
#' - `RHOHV`: Correlation coefficient (unitless). Correlation between vertically
#'   polarized and horizontally polarized reflectivity factor
#' - `PHIDP`: Differential phase (degrees)
#' - `ZDR`: (Logged) differential reflectivity (dB)
#' The scan parameters are named according to the OPERA data information
#' model (ODIM), see Table 16 in the
#' [ODIM specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf).
#'
#' @examples
#' # load an example scan:
#' data(example_scan)
#'
#' # print to screen the available scan parameters
#' summary(example_scan)
#'
#' # make ppi for the scan
#' # plot the reflectivity param:
#' plot(example_scan, param = "DBZH")
#' \dontrun{
#' # change the range of reflectivities to plot, from -10 to 10 dBZ:
#' plot(example_scan, param = "DBZH", zlim = c(-10, 10))
#'
#' # change the scale name and colour scheme, using viridis colors:
#' plot(example_scan, param = "DBZH", zlim = c(-10, 10)) + viridis::scale_fill_viridis(name = "dBZ")
#' }
plot.scan <- function(x, param, xlim = c(0, 100000),
                      ylim = c(0, 360), zlim = c(-20, 20), na.value = "transparent", ...) {
  stopifnot(inherits(x, "scan"))

  if (hasArg("quantity")) stop("unknown function argument 'quantity`. Did you mean `param`?")

  if (missing(param)) {
    if ("DBZH" %in% names(x$params)) {
      param <- "DBZH"
    } else {
      param <- names(x$params)[1]
    }
  } else if (!see_if(param %in% names(x$params))) {
    stop(paste("parameter '", param, "' not found in scan", sep = ""))
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

  rscale <- x$geo$rscale
  ascale <- x$geo$ascale
  rstart <- ifelse(is.null(x$geo$rstart), 0, x$geo$rstart)
  astart <- ifelse(is.null(x$geo$astart), 0, x$geo$astart)

  data <- raster::as.data.frame(raster::flip(raster(t(data), ymn = astart, ymx = astart + 360, xmn = rstart, xmx = rstart + rscale * dimraster[1]), direction = "y"), xy = T)
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
  azimuth <- NULL # dummy assignment to suppress devtools check warning
  bbox <- coord_cartesian(xlim = xlim, ylim = ylim)
  ggplot(data = data, ...) +
    geom_raster(aes(x = range, y = azimuth, fill = eval(parse(text = param)))) +
    colorscale +
    bbox
}
