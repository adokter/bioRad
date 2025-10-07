#' Add scan parameter from a georeferenced raster
#'
#' @param x A `pvol` or `scan` object.
#' @param raster  An object of class `terra::SpatRaster` or `raster::Raster`.
#' @param param The name of the added parameter.
#'
#' @return The object with an added parameter, extracting data from the raster
#' specified by `raster`.
#'
#' @export
add_param <- function(x, raster, param) {
  UseMethod("add_param", x)
}

#' @rdname add_param
#'
#' @export
add_param.scan <- function(scan, raster, param){
  stopifnot(inherits(x, "scan"))
  data=raster::extract(raster(raster),scan_to_spatial(scan))
  scan$params[[param]] = matrix(data,nrow=dim(scan$params[[1]]))
  attributes(scan$params[[param]])=attributes(scan$params[[1]])
  attributes(scan$params[[param]])$param=param
  scan
}

#' @rdname add_param
#'
#' @export
add_param.pvol <- function(pvol, raster, param){
  lat=pvol$attributes$where$lat
  lon=pvol$attributes$where$lon

  # make sure the raster is in the coordinate system of polar scan parameters
  # this will speed up the data extraction
  localCrs=paste("+proj=aeqd +lat_0=", lat," +lon_0=", lon, " +units=m", sep = "")
  raster |> project(localCrs) -> raster_local

  pvol$scans=lapply(pvol$scans, function(x) add_param.scan(x, raster=raster_local, param=param))
  pvol
}


