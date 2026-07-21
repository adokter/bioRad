#' Add scan parameter from a georeferenced raster.
#'
#' This function adds data from a georeferenced raster file (either in `terra::spatraster`
#' or `raster::RasterLayer` format) to a single `scan` or to all scans in a `pvol` object.
#' This is specifically useful for adding digital elevation information to these objects
#' when creating vertical profiles relative to ground level, which requires information for
#' each range gate on the topographic ground level height.
#' @param x A `pvol` or `scan` object.
#' @param raster  An object of class `terra::SpatRaster` or `raster::RasterLayer`.
#' @param param The name of the added parameter.
#' @returns The object `x` with an added parameter, extracting data from the raster
#' specified by `raster`.
#' @family scan manipulation functions
#' @export
#' @examples
#' # locate example volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # load the file:
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # the following code block downloads digital elevation data from the internet
#' \donttest{
#' if(requireNamespace("elevatr", quietly = TRUE)){
#' # download digital elevation information:
#' example_pvol |>
#'   # extract lowest scan
#'   get_scan(.5) |>
#'   # convert to raster object
#'   scan_to_raster(param="DBZH") |>
#'   # convert to terra raster class
#'   terra::rast() |>
#'   # download digital elevation data (increase z for higher resolutions)
#'   elevatr::get_elev_raster(z = 5, clip = "bbox") -> data_dem
#' # set digital elevations for open water to mean sea level (0)
#' data_dem[data_dem<0]=0
#' # set an informative name for the DEM information
#' names(data_dem) <- "HGHT"
#'
#' # add the DEM information as a scan parameter to the polar volume:
#' example_pvol <- add_param(example_pvol, data_dem, "HGHT")
#'
#' # verify that HGHT parameter has been added:
#' get_scan(example_pvol,.5)
#'
#' # plot the digital elevation paired with the lowest scan:
#' example_pvol |>
#'   get_scan(.5) |>
#'   project_as_ppi() |>
#'   map(param="HGHT", zlim=c(0,200), palette = viridis::viridis(100))
#' }
#' }
add_param <- function(x, raster, param) {
  UseMethod("add_param", x)
}

#' @rdname add_param
#' @export
add_param.scan <- function(x, raster, param){
  stopifnot(inherits(x, "scan"))

  # raster::raster() drops the values of an in-memory RasterLayer; converting to
  # a terra SpatRaster first ensures the values are retained during extraction.
  if (inherits(raster, "RasterLayer")) raster <- terra::rast(raster)

  extent <- terra::ext(raster)
  distance_max <- max(sqrt(extent$xmin^2 + extent$ymin^2),sqrt(extent$xmax^2 + extent$ymax^2))
  range_max <- beam_range(distance_max, elev=x$geo$elangle, lat=x$geo$lat)

  spdf <- scan_to_spatial(x)
  # do not consider ranges outside the raster
  idx_calc <- spdf$range-x$geo$rscale/2 < range_max

  # number of range gates dropped
  #n_dropped_ranges = dim(x)[2]-ceiling(range_max / x$geo$rscale)
  #data=c(data, rep(NA,n_dropped_ranges*360/x$geo$ascale))

  data=rep(NA,nrow(spdf))
  data[idx_calc]=raster::extract(raster::raster(raster),spdf[idx_calc,])

  x$params[[param]] = matrix(data,nrow=dim(x$params[[1]])[1])
  attributes(x$params[[param]])=attributes(x$params[[1]])
  attributes(x$params[[param]])$param=param
  x
}

#' @rdname add_param
#' @export
add_param.pvol <- function(x, raster, param){
  # make sure the raster is in the coordinate system of polar scan parameters
  # this will speed up the data extraction
  localCrs=paste("+proj=aeqd +lat_0=", x$attributes$where$lat," +lon_0=", x$attributes$where$lon, " +units=m", sep = "")
  if(inherits(raster,"RasterLayer")) raster = terra::rast(raster)
  raster |> terra::project(localCrs) -> raster_local

  x$scans=lapply(x$scans, function(x) add_param.scan(x, raster=raster_local, param=param))
  x
}
