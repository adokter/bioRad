color_scale <- function(param, zlim) {
  if (param %in% c("VRADH", "VRADV", "VRAD")) {
    colorscale <- scale_colour_gradient2(low = "blue", high = "red",
                                         mid = "white", name = param,
                                         midpoint = 0, limits = zlim)
  } else {
    colorscale <- scale_colour_gradientn(colours = c("lightblue", "darkblue",
                                                     "green", "yellow", "red",
                                                     "magenta"),
                                         name = param, limits = zlim)
  }
  return(colorscale)
}

color_scale_fill <- function(param, zlim) {
  if (param %in% c("VRADH", "VRADV", "VRAD")) {
    colorscale <- scale_fill_gradient2(low = "blue", high = "red",
                                       mid = "white", name = param,
                                       midpoint = 0, limits = zlim)
  } else {
    colorscale <- scale_fill_gradientn(colours = c("lightblue", "darkblue",
                                                   "green", "yellow", "red",
                                                   "magenta"),
                                       name = param, limits = zlim)
  }
  return(colorscale)
}

# helper function to add transparency
# class dispatching needs improvement
add_color_transparency <- function(color, alpha = 1) {
  if (missing(color)) {
    stop("Please provide a vector or matrix of colours.")
  }

  mycol <- col2rgb(color)/255
  mycol <- rgb(mycol[1,], mycol[2,], mycol[3,], alpha = alpha)
  if (inherits(color, "ggmap")) {
    mycol <- matrix(mycol, nrow = dim(color)[1], ncol = dim(color)[2])
    attributes(mycol) <- attributes(color)
    class(mycol) <- class(color)
    return(mycol)
  } else if (inherits(color, "raster")) {
    color@data@values <- mycol
    return(color)
  }
  else{
    return(mycol)
    #apply(sapply(color, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))
  }
}

# color scale used in vertical profile plots:
r_points <- c(1, 63, 82, 94, 146, 177, 192, 209, 256)
r_values <- c(255, 255, 163, 255, 255, 81, 81, 0, 0)
g_points <- c(1, 65, 80, 111, 143, 256)
g_values <- c(255, 255, 163, 163, 0, 0)
b_points <- c(1, 80, 97, 111, 128, 160, 207, 256)
b_values <- c(255, 0, 0, 82, 0, 0, 255, 0)
plot_colors <- rgb(c(200, approx(r_points, r_values,
                                seq(1, 256, length.out = 255))$y),
                  c(200, approx(g_points, g_values,
                                seq(1, 256, length.out = 255))$y),
                  c(200, approx(b_points, b_values,
                                seq(1, 256, length.out = 255))$y),
                  maxColorValue = 255)
