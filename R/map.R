#' Map a plan position indicator (`ppi`)
#'
#' Plot a ppi on a Stamen Maps, OpenStreetMap, Google Maps or Naver Map base
#' layer map using [ggmap][ggmap::ggmap].
#'
#' @param x An object of class `ppi`.
#' @param map  The basemap to use, result of a call to [download_basemap].
#' @param param The scan parameter to plot.
#' @param alpha Transparency of the data, value between 0 and 1.
#' @param radar_size Size of the symbol indicating the radar position.
#' @param radar_color Color of the symbol indicating the radar position.
#' @param n_color The number of colors (>=1) to be in the palette.
#' @param palette (Optional) character vector of hexadecimal color values defining
#' the plot color scale, e.g. output from [viridis][viridisLite::viridis]
#' @param xlim Range of x values to plot (degrees longitude), as atomic
#' vector of length 2.
#' @param ylim Range of y values to plot (degrees latitude), as an atomic
#' vector of length 2.
#' @param zlim The range of values to plot.
#' @param ratio Aspect ratio between x and y scale, by default
#' \eqn{1/cos(latitude radar * pi/180)}.
#' @param ... Arguments passed to low level [ggmap][ggmap::ggmap] function.
#' @param radar.size Deprecated argument, use radar_size instead.
#' @param radar.color Deprecated argument, use radar_color instead.
#' @param n.color Deprecated argument, use n_color instead.
#'
#' @return A ggmap object (a classed raster object with a bounding
#' box attribute).
#'
#' @details
#' Available scan parameters for mapping can by printed to screen by
#' `summary(x)`. Commonly available parameters are:
#' * `DBZH`, `DBZ`: (Logged) reflectivity factor (dBZ)
#' * `TH`, `T`: (Logged) uncorrected reflectivity factor (dBZ)
#' * `VRADH`, `VRAD`: Radial velocity (m/s). Radial velocities towards the radar
#'   are negative, while radial velocities away from the radar are positive
#' * `RHOHV`: Correlation coefficient (unitless) Correlation between vertically
#'   polarized and horizontally polarized reflectivity factor
#' * `PHIDP`: Differential phase (degrees)
#' * `ZDR`: (Logged) differential reflectivity (dB)
#' The scan parameters are named according to the OPERA data information
#' model (ODIM), see Table 16 in the
#' [ODIM specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf).
#'
#' @export
#' @examples
#' # load an example scan:
#' data(example_scan)
#' # make ppi's for all scan parameters in the scan
#' ppi <- project_as_ppi(example_scan)
#' \dontrun{
#' # grab a basemap that matches the extent of the ppi:
#' # using a gray-scale basemap:
#' basemap <- download_basemap(ppi, maptype = "toner-lite")
#'
#' # map the radial velocity scan parameter onto the basemap:
#' map(ppi, map = basemap, param = "VRADH")
#'
#' # extend the plotting range of velocities, from -50 to 50 m/s:
#' map(ppi, map = basemap, param = "VRADH", zlim = c(-50, 50))
#'
#' # map the reflectivity on a terrain basemap:
#' basemap <- download_basemap(ppi, maptype = "terrain")
#' map(ppi, map = basemap, param = "DBZH")
#'
#' # change the color palette, e.g. Viridis colors:
#' map(ppi, map = basemap, param = "DBZH", palette = viridis::viridis(100), zlim=c(-10,10))
#'
#' # give the data more transparency:
#' map(ppi, map = basemap, param = "DBZH", alpha = 0.3)
#'
#' # change the appearance of the symbol indicating the radar location:
#' map(ppi, map = basemap, radar_size = 5, radar_color = "blue")
#'
#' # crop the map:
#' map(ppi, map = basemap, xlim = c(12.4, 13.2), ylim = c(56, 56.5))
#' }
map <- function(x, ...) {
  UseMethod("map", x)
}

#' @describeIn map plot a 'ppi' object on a map
#' @export
map.ppi <- function(x, map, param, alpha = 0.7, xlim, ylim,
                    zlim = c(-20, 20), ratio, radar_size = 3,
                    radar_color = "red", n_color = 1000,
                    radar.size = 3, radar.color = "red",
                    n.color = 1000, palette = NA, ...) {

  # deprecate function arguments
  if (!missing(radar.size)) {
    warning("argument radar.size is deprecated; please use radar_size instead.",
      call. = FALSE
    )
    radar_size <- radar.size
  }
  if (!missing(radar.color)) {
    warning("argument radar.color is deprecated; please use radar_color instead.",
      call. = FALSE
    )
    radar_color <- radar.color
  }
  if (!missing(n.color)) {
    warning("argument n.color is deprecated; please use n_color instead.",
      call. = FALSE
    )
    n_color <- n.color
  }

  stopifnot(inherits(x, "ppi"))

  if (hasArg("quantity")) stop("unknown function argument 'quantity`. Did you mean `param`?")

  if (missing(param)) {
    if ("DBZH" %in% names(x$data)) {
      param <- "DBZH"
    } else {
      param <- names(x$data)[1]
    }
  } else if (!is.character(param)) {
    stop(
      "'param' should be a character string with a valid ",
      "scan parameter name."
    )
  }
  if (missing(zlim)) {
    zlim <- get_zlim(param, zlim)
  }
  if (!(param %in% names(x$data))) {
    stop(paste("no scan parameter '", param, "' in this ppi", sep = ""))
  }
  if (is.na(raster::crs(x$data))) {
    stop("Not a projected ppi, map() expects a ppi generated by project_as_ppi() with argument project=TRUE")
  }
  if (!attributes(map)$ppi) {
    stop("Not a ppi map, use download_basemap() to download a map.")
  }
  if (attributes(map)$geo$lat != x$geo$lat ||
    attributes(map)$geo$lon != x$geo$lon) {
    stop("Not a basemap for this radar location.")
  }

  # set color scales and palettes
  if (!are_equal(palette, NA)) {
    if(!(is.character(palette) && length(palette) > 1)) stop("palette should be a character vector with hex color values")
    # apply transparancy
    palette <- alpha(palette,alpha)
    n_color = length(palette)
    colorscale <- color_palette_to_scale_colour(param, zlim, palette, na.value = "transparent")
  }
  else{
    palette <- color_palette(param = param, n_color = n_color, alpha = alpha)
    colorscale <- color_scale(param, zlim)
  }

  # extract the scan parameter
  data <- do.call(function(y) x$data[y], list(param))
  wgs84 <- CRS("+proj=longlat +datum=WGS84")
  epsg3857 <- CRS("+init=epsg:3857") # this is the google mercator projection
  mybbox <- suppressWarnings(
    spTransform(
      SpatialPoints(t(data@bbox),
        proj4string = data@proj4string
      ),
      CRS("+init=epsg:3857")
    )
  )
  mybbox.wgs <- suppressWarnings(
    spTransform(
      SpatialPoints(t(data@bbox),
        proj4string = data@proj4string
      ),
      wgs84
    )
  )
  e <- raster::extent(mybbox.wgs)
  r <- raster(raster::extent(mybbox),
    ncol = data@grid@cells.dim[1] * .9,
    nrow = data@grid@cells.dim[2] * .9, crs = CRS(proj4string(mybbox))
  )

  # convert to google earth mercator projection
  data <- suppressWarnings(
    as.data.frame(spTransform(data, CRS("+init=epsg:3857")))
  )
  # bring z-values within plotting range
  index <- which(data$z < zlim[1])
  if (length(index) > 0) {
    data[index, ]$z <- zlim[1]
  }
  index <- which(data$z > zlim[2])
  if (length(index) > 0) {
    data[index, ]$z <- zlim[2]
  }

  # rasterize data on the raster, if there is valid data
  if(FALSE %in% is.na(data[,1])){
    r <- raster::rasterize(data[, 2:3], r, data[, 1])
  } else{
    raster::values(r) <- NA
  }

  # function to convert values to hex color strings
  col_func <- function(value, lim) {
    output <- rep(0, length(value))
    output <- round((value - lim[1]) / (lim[2] - lim[1]) * n_color)
    output[output > n_color] <- n_color
    output[output < 1] <- 1
    return(palette[output])
  }

  # convert data values to hex color string values.
  r@data@values <- col_func(r@data@values, zlim)

  # these declarations prevent generation of NOTE "no visible binding for
  # global variable" during package Check
  lon <- lat <- y <- z <- NA
  # extract unique radar locations
  latlon_radar <- unique(data.frame(lat=c(x$geo$lat), lon=c(x$geo$lon)))
  # symbols for the radar position
  # dummy is a hack to be able to include the ggplot2 color scale,
  # radarpoint is the actual plotting of radar positions.
  dummy <- geom_point(aes(x = lon, y = lat, colour = z),
    size = 0,
    data = data.frame(
      lon = latlon_radar$lon,
      lat = latlon_radar$lat,
      z = 0
    )
  )
  radarpoint <- geom_point(aes(x = lon, y = lat),
    colour = radar_color,
    size = radar_size,
    data = data.frame(lon = latlon_radar$lon, lat = latlon_radar$lat)
  )
  # bounding box
  bboxlatlon <- attributes(map)$geo$bbox
  # remove dimnames, otherwise ggmap will give a warning message below:
  dimnames(bboxlatlon) <- NULL
  if (missing(xlim)) xlim <- bboxlatlon[1, ]
  if (missing(ylim)) ylim <- bboxlatlon[2, ]
  # plot the data on the map
  mymap <- suppressMessages(
    ggmap(map) +
      inset_raster(raster::as.matrix(r), e@xmin, e@xmax, e@ymin, e@ymax) +
      dummy + colorscale +
      radarpoint +
      scale_x_continuous(limits = xlim, expand = c(0, 0)) +
      scale_y_continuous(limits = ylim, expand = c(0, 0))
  )
  suppressWarnings(mymap)

}
