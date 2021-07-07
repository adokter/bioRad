#' Plot a scan (`scan`) in polar coordinates
#'
#' Plots a scan (`scan`) in polar coordinates. To plot in Cartesian coordinates,
#' see [project_to_ppi()].
#'
#' @param x A `scan` object.
#' @param param Character. Scan parameter to plot, e.g. `DBZH` or `VRADH`. See
#'   [summary.param()] for commonly available parameters.
#' @param xlim Numeric vector of length 2. Range of x values (range, distance to
#'   radar) to plot.
#' @param ylim Numeric vector of length 2. Range of y values (azimuth) to plot.
#' @param zlim Numeric vector of length 2. The range of parameter values to
#'   plot.
#' @param na.value Character. [ggplot2::ggplot()] parameter to set the color of
#'   `NA` values.
#' @param ... Arguments passed to [ggplot2::ggplot()].
#'
#' @method plot scan
#'
#' @export
#'
#' @examples
#' # Plot reflectivity
#' plot(example_scan, param = "DBZH")
#' \dontrun{
#' # Change the range of reflectivities to plot, from -10 to 10 dBZ
#' plot(example_scan, param = "DBZH", zlim = c(-10, 10))
#'
#' # Change the scale name, change the color palette to Viridis colors
#' plot(example_scan, param = "DBZH", zlim = c(-10, 10)) +
#'   viridis::scale_fill_viridis(name = "dBZ")
#' }
plot.scan <- function(x, param, xlim = c(0, 100000),
                      ylim = c(0, 360), zlim = c(-20, 20), na.value = "transparent", ...) {
  stopifnot(inherits(x, "scan"))

  if (hasArg("quantity")) stop("Unknown function argument `quantity`. Did you mean `param`?")

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
