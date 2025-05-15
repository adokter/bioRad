#' Plot a ppiRaster object using ggplot2
#'
#' @include ppi_raster.R
#' @param x A ppiRaster object
#' @param y Missing (for S4 method signature)
#' @param param Character. Layer name to plot (default first layer)
#' @param zlim Numeric[2]. Value range (default infra-parameter limits)
#' @param xlim,ylim Numeric[2]. Spatial extents (default raster extent)
#' @param ratio Numeric. Aspect ratio y/x (default 1)
#' @param na.value Color for NA values (default "transparent")
#' @param ... Additional arguments passed to ggplot2::ggplot
#' @rdname ppiRaster-class
#' @aliases plot,ppiRaster,missing-method,plot.ppiRaster
#' @import ggplot2 terra
#' @importFrom scales hue_pal
#' @exportMethod plot
setMethod(
  "plot",
  signature(x = "ppiRaster", y = "missing"),
  function(x, y, param, zlim, xlim, ylim, ratio = 1, na.value = "transparent", ...) {
    # choose parameter
    if (missing(param)) param <- names(x)[1]
    if (!param %in% names(x)) stop("layer '", param, "' not found in ppiRaster")

    # determine per-parameter defaults
    if (missing(zlim)) {
      # use same logic as original plot.ppi
      zlim <- get_zlim(param, NULL)
    }
    # build color scale
    colorscale <- color_scale_fill(param, zlim, na.value)

    # extract and clamp values
    r <- terra::subset(x, param)
    vals <- terra::values(r, mat = FALSE)
    vals[vals < zlim[1]] <- zlim[1]
    vals[vals > zlim[2]] <- zlim[2]
    terra::values(r) <- vals

    # convert to data.frame
    df <- as.data.frame(r, xy = TRUE)
    names(df)[3] <- "value"

    # spatial limits
    e <- terra::ext(r)
    if (missing(xlim)) xlim <- c(e[1], e[2])
    if (missing(ylim)) ylim <- c(e[3], e[4])

    # plot with original color scale
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = value), ...) +
      ggplot2::geom_raster(na.rm = TRUE) +
      colorscale +
      ggplot2::coord_fixed(xlim = xlim, ylim = ylim, ratio = ratio)
  }
)
