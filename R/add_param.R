#' Add scan parameter from a georeferenced raster
#'
#' @param x A `pvol` or `scan` object.
#' @param raster  An object of class `terra::SpatRaster` or `raster::RasterLayer`.
#' @param param The name of the added parameter.
#'
#' @return The object `x` with an added parameter, extracting data from the raster
#' specified by `raster`.
#'
#' @export
add_param <- function(x, raster, param) {
  UseMethod("add_param", x)
}

#' @rdname add_param
#'
#' @export
add_param.scan <- function(x, raster, param){
  stopifnot(inherits(x, "scan"))

  extent <- ext(raster)
  distance_max <- max(sqrt(extent$xmin^2 + extent$ymin^2),sqrt(extent$xmax^2 + extent$ymax^2))
  range_max <- beam_range(distance_max, elev=x$geo$elangle, lat=x$geo$lat)

  spdf <- scan_to_spatial(x)
  # do not consider ranges outside the raster
  idx_calc <- spdf$range-x$geo$rscale/2 < range_max

  # number of range gates dropped
  #n_dropped_ranges = dim(x)[2]-ceiling(range_max / x$geo$rscale)
  #data=c(data, rep(NA,n_dropped_ranges*360/x$geo$ascale))

  data=rep(NA,nrow(spdf))
  data[idx_calc]=raster::extract(raster(raster),spdf[idx_calc,])

  x$params[[param]] = matrix(data,nrow=dim(x$params[[1]])[1])
  attributes(x$params[[param]])=attributes(x$params[[1]])
  attributes(x$params[[param]])$param=param
  x
}

#' @rdname add_param
#'
#' @export
add_param.pvol <- function(x, raster, param){
  # make sure the raster is in the coordinate system of polar scan parameters
  # this will speed up the data extraction
  localCrs=paste("+proj=aeqd +lat_0=", x$attributes$where$lat," +lon_0=", x$attributes$where$lon, " +units=m", sep = "")
  if(inherits(raster,"RasterLayer")) raster = rast(raster)
  raster |> project(localCrs) -> raster_local

  x$scans=lapply(x$scans, function(x) add_param.scan(x, raster=raster_local, param=param))
  x
}


