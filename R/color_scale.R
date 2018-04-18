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
  if (param %in% c("VRADH","VRADV","VRAD")) {
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
add_color_transparency <- function(col, alpha = 1) {
  if (missing(col)) {
    stop("Please provide a vector or matrix of colours.")
  }

  mycol <- col2rgb(col)/255
  mycol <- rgb(mycol[1,], mycol[2,], mycol[3,], alpha = alpha)
  if (inherits(col,"ggmap")) {
    mycol <- matrix(mycol, nrow = dim(col)[1], ncol = dim(col)[2])
    attributes(mycol) <- attributes(col)
    class(mycol) <- class(col)
    return(mycol)
  } else if (inherits(col, "raster")) {
    col@data@values <- mycol
    return(col)
  }
  else{
    return(mycol)
    #apply(sapply(col, col2rgb)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha=alpha))
  }
}

# color scale used in vertical profile plots:
r.points = c(1, 63, 82, 94, 146, 177, 192, 209, 256)
r.values = c(255, 255, 163, 255, 255, 81, 81, 0, 0)
g.points = c(1, 65, 80, 111, 143, 256)
g.values = c(255, 255, 163, 163, 0, 0)
b.points = c(1, 80, 97, 111, 128, 160, 207, 256)
b.values = c(255, 0, 0, 82, 0, 0, 255, 0)
plot.colors = rgb(c(200, approx(r.points, r.values,
                                seq(1, 256, length.out = 255))$y),
                  c(200, approx(g.points, g.values,
                                seq(1, 256, length.out = 255))$y),
                  c(200, approx(b.points, b.values,
                                seq(1, 256, length.out = 255))$y),
                  maxColorValue = 255)
