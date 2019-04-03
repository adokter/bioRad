#' convert a polar scan into a spatial object.
#'
#' Georeferences the pixels of a scan into a SpatialPointsDataFrame object.
#'
#' @inheritParams beam_height
#' @param scan a scan (sweep) of class scan
#' @param lat Geodetic latitude of the radar in degrees. If missing taken from \code{scan}.
#' @param lon Geodetic longitude of the radar in degrees. If missing taken from \code{scan}.
#' @return a SpatialPointsDataFrame
#' @export
#' @details Beam altitude accounts for the curvature of the earth, using \link{beam_height}.
#' Distance from the radar over the earth's surface is calculated using \link{beam_distance}.
scan_to_spatial <- function(scan, lat, lon, k = 4 / 3, re = 6378, rp = 6357) {
  assert_that(is.scan(scan))
  assert_that(is.number(k))
  assert_that(is.number(re))
  assert_that(is.number(rp))
  if(is.null(scan$geo$lat) && missing(lat)) stop("radar latitude cannot be found in scan, specify using 'lat' argument")
  if(is.null(scan$geo$lon) && missing(lon)) stop("radar longitude cannot be found in scan, specify using 'lon' argument")
  if(missing(lat)) lat=scan$geo$lat
  if(missing(lon)) lon=scan$geo$lon
  assert_that(is.number(lat))
  assert_that(is.number(lon))

  proj4string <- CRS(paste("+proj=aeqd +lat_0=", lat,
                           " +lon_0=", lon,
                           " +units=m",sep = ""))

  rscale=scan$geo$rscale
  ascale=scan$geo$ascale
  elev <- scan$geo$elangle

  data=data.frame(azim=c(t(matrix(rep(seq(0,dim(scan)[3]-1)*ascale,dim(scan)[2]),nrow=dim(scan)[3]))),
                  range=rep(seq(1,dim(scan)[2])*rscale,dim(scan)[3]),
                  distance=beam_distance(range=rep(seq(1,dim(scan)[2])*rscale,dim(scan)[3]),elev=elev, k=k, lat=lat, re=re, rp=rp))
  data$HGHT=scan$geo$height + beam_height(data$range,elev,k=k,lat=lat,re=re,rp=rp)
  data=cbind(data,as.data.frame(sapply(scan$params,c)))
  coords=data.frame(x=data$distance*cos(pi/2-data$azim*pi/180),
                    y=data$distance*sin(pi/2-data$azim*pi/180))
  SpatialPointsDataFrame(coords=coords,data=data,coords.nrs=c(3,4),proj4string=proj4string)
}

#' convert a polar scan into a raster
#'
#' convert an object of class 'scan' into a raster of class 'RasterBrick'
#' @inheritParams scan_to_spatial
#' @param nx number of raster pixels in the x (longitude) dimension
#' @param ny number of raster pixels in the y (latitude) dimension
#' @param xlim x (longitude) range
#' @param ylim y (latitude) range
#' @param param scan parameters to include. If \code{NA} include all scan parameters. Reducing the number
#' of scan parameters speeds up evaluation.
#' @param crs character or object of class CRS. PROJ.4 type description of a Coordinate Reference System (map projection).
#' When 'NA' (default), an azimuthal equidistant projection with origin at the radar location is used.
#' To use a WSG84 (lat,lon) projection, use crs="+proj=longlat +datum=WGS84"
#' @param res numeric vector of length 1 or 2 to set the resolution of the raster (see \link[raster]{res}).
#' If this argument is used, arguments \code{nx} and \code{ny} are ignored. Unit is identical to \code{xlim} and \code{ylim}
#' @return a RasterBrick
#' @details uses \link{scan_to_spatial} to georeference the scan's pixels. If multiple scan pixels fall within
#' the same raster pixel, the last added pixel is given (see \link[raster]{rasterize} for details).
#' @export
#' @examples
#' # default projects full extent on 100x100 pixel raster:
#' scan_to_raster(example_scan)
#' # crop the scan and project at a resolution of 0.1 degree:
#' scan_to_raster(example_scan, ylim=c(55,57),xlim=c(12,13), res=.1)
scan_to_raster <- function(scan,nx=100,ny=100,xlim,ylim,res=NA,param,lat,lon,crs=NA, k = 4 / 3,  re = 6378, rp = 6357){
  if(!is.scan(scan)) stop("'scan' should be an object of class scan")
  if(!is.number(nx) && missing(res)) stop("'nx' should be an integer")
  if(!is.number(ny) && missing(res)) stop("ny' should be an integer")

  if(!missing(xlim)){
    if(length(xlim)!=2 & !is.numeric(xlim)) stop("'xlim' should be an integer vector of length two")
    if(is.na(xlim[1]) | is.na(xlim[2]) | xlim[1]>xlim[2]) stop("'xlim' should be a vector with two numeric values for upper and lower bound")
  }
  if(!missing(ylim)){
    if(length(ylim)!=2 & !is.numeric(ylim)) stop("'ylim' should be an integer vector of length two")
    if(is.na(ylim[1]) | is.na(ylim[2]) | ylim[1]>ylim[2]) stop("'ylim' should be a vector with two numeric values for upper and lower bound")
  }
  if(!missing(res) && !is.na(res)){
    assert_that(is.numeric(res))
    assert_that(length(res)<=2)
  }
  if(!missing(param)){
    if(FALSE %in% (param %in% c(names(scan$params), "azim","range","distance"))) stop("'param' contains scan parameter not found in scan")
    if(!(FALSE %in% (param %in% c("azim","range","distance")))) stop("'param' should contain the name of one or more scan parameters contained in 'scan'")

    param_to_use=param
  }
  else{
    param_to_use=names(scan$params)
  }

  if(is.null(scan$geo$lat) && missing(lat)) stop("radar latitude cannot be found in scan, specify using 'lat' argument")
  if(is.null(scan$geo$lon) && missing(lon)) stop("radar longitude cannot be found in scan, specify using 'lon' argument")

  if(missing(lat)) lat=scan$geo$lat
  if(missing(lon)) lon=scan$geo$lon

  assert_that(is.number(lat))
  assert_that(is.number(lon))

  if(missing(crs) | is.na(crs)){
    crs <- CRS(paste("+proj=aeqd +lat_0=", lat,
                     " +lon_0=", lon,
                     " +units=m",sep = ""))

  }
  else{
    # check crs argument as in raster::raster()
    crs=CRS(as.character(raster::projection(crs)))
  }

  assert_that(is.number(k))
  assert_that(is.number(re))
  assert_that(is.number(rp))

  rscale=scan$geo$rscale
  ascale=scan$geo$ascale

  nrang <- dim(scan)[2]
  nazim <- dim(scan)[3]

  # extent not fully specified, determine it
  if(missing(xlim) | missing(ylim)){
    # georeference the data
    spdf=scan_to_spatial(scan, k = k, lat=lat, lon=lon, re=re, rp=rp)
    # keep only selected scan parameters
    if(!missing(param)) spdf=spdf[param]
    # transform spatialpoints to coordinate system of the raster
    if(!missing(crs)) spdf=spTransform(spdf,crs)
    # get extent of the available data
    spdf_extent=raster::extent(spdf)
    # prepare a raster matching the data extent (or user-specified extent)
    if(missing(xlim)) xlim=c(spdf_extent@xmin,spdf_extent@xmax)
    if(missing(ylim)) ylim=c(spdf_extent@ymin,spdf_extent@ymax)
  }

  if(missing(res) | is.na(res)){
    r <- raster(ncols=nx, nrows=ny,ext=raster::extent(c(xlim,ylim)),crs=crs)
  }
  else{
    r <- raster(ncols=nx, nrows=ny,ext=raster::extent(c(xlim,ylim)),crs=crs,res=res)
  }
  # convert raster coordinates to polar indices
  polar_coords=cartesian_to_polar(coordinates(r),elev=scan$geo$elangle,k=k, lat=lat, re=re, rp=rp)
  index=polar_to_index(polar_coords,rangebin=rscale,azimbin=ascale)
  # set indices outside the scan's matrix to NA
  index$row[index$row>nrang]=NA
  index$col[index$col>nazim]=NA
  # convert 2D index to 1D index
  index=(index$col-1)*nrang+index$row

  # generate brick for each scan parameter
  param_to_add=setdiff(param_to_use, c("distance","range","azim"))
  output=raster::brick(r,nl=length(param_to_add),filename='')
  names(output)=param_to_add
  # fill the rasterbrick
  for(name in param_to_add){
    # suppress warning 'In readAll(x) : cannot read values; there is no file associated with this RasterBrick
    suppressWarnings({ raster::values(output[[name]]) <- scan$params[[name]][index] })
  }
  if("distance" %in% param_to_use) output$distance=beam_distance(polar_coords$range,elev=scan$geo$elangle,k=k, lat=lat, re=re, rp=rp)
  if("range" %in% param_to_use) output$range=polar_coords$range
  if("azim" %in% param_to_use) output$azim=polar_coords$azim
  output
}


