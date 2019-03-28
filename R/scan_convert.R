#' convert a polar scan into a spatial object.
#'
#' Georeferences the pixels of a scan into a SpatialPointsDataFrame object.
#'
#' @inheritParams beam_height
#' @param scan a scan (sweep) of class scan
#' @param lat Geodetic latitude of the radar in degrees. If \code{NA} taken from \code{scan}.
#' @param lon Geodetic longitude of the radar in degrees. If \code{NA} taken from \code{scan}.
#' @return a SpatialPointsDataFrame
#' @export
#' @details Beam altitude accounts for the curvature of the earth, using \link{beam_height}.
#' Distance from the radar over the earth's surface is calculated using \link{beam_distance}.
scan_to_spatial <- function(scan, lat=NA, lon=NA, k = 4 / 3, re = 6378, rp = 6357) {
  assert_that(is.scan(scan))
  assert_that(is.number(k))
  assert_that(is.number(re))
  assert_that(is.number(rp))
  if(is.null(scan$geo$lat) && is.na(lat)) stop("radar latitude cannot be found in scan, specify using 'lat' argument")
  if(is.null(scan$geo$lon) && is.na(lon)) stop("radar longitude cannot be found in scan, specify using 'lon' argument")
  if(is.number(lat)) scan$geo$lat=lat
  if(is.number(lon)) scan$geo$lon=lon

  proj4string <- CRS(paste("+proj=aeqd +lat_0=", scan$geo$lat,
                           " +lon_0=", scan$geo$lon,
                           " +units=m",sep = ""))

  rscale=scan$geo$rscale
  ascale=scan$geo$ascale
  elev <- scan$geo$elangle

  data=data.frame(azim=c(t(matrix(rep(seq(0,dim(scan)[3]-1)*ascale,dim(scan)[2]),nrow=dim(scan)[3]))),
                  range=rep(seq(1,dim(scan)[2])*rscale,dim(scan)[3]),
                  distance=beam_distance(range=rep(seq(1,dim(scan)[2])*rscale,dim(scan)[3]),elev=elev, k=k, lat=scan$geo$lat, re=re, rp=rp))
  data$HGHT=scan$geo$height + beam_height(data$range,elev,k=k,lat=scan$geo$lat,re=re,rp=rp)
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
#' By default a WSG84 (lat,lon) projection.
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
scan_to_raster <- function(scan,nx=100,ny=100,xlim=NA,ylim=NA,res=NA,param=NA,lat=NA, lon=NA,crs="+proj=longlat +datum=WGS84", k = 4 / 3,  re = 6378, rp = 6357){
  if(!is.scan(scan)) stop("'scan' should be an object of class scan")
  if(!is.number(nx) && is.na(res)) stop("'nx' should be an integer")
  if(!is.number(ny) && is.na(res)) stop("ny' should be an integer")
  if(!are_equal(xlim,NA)){
    if(length(xlim)!=2 & !is.numeric(xlim)) stop("'xlim' should be an integer vector of length two")
    if(is.na(xlim[1]) | is.na(xlim[2]) | xlim[1]>xlim[2]) stop("'xlim' should be a vector with two numeric values for upper and lower bound")
  }
  if(!are_equal(ylim,NA)){
    if(length(ylim)!=2 & !is.numeric(ylim)) stop("'ylim' should be an integer vector of length two")
    if(is.na(ylim[1]) | is.na(ylim[2]) | ylim[1]>ylim[2]) stop("'ylim' should be a vector with two numeric values for upper and lower bound")
  }
  if(!are_equal(res,NA)){
    assert_that(is.numeric(res))
    assert_that(length(res)<=2)
  }
  if(!are_equal(param,NA)){
    if(FALSE %in% (param %in% names(scan$params))) stop("'param' contains scan parameter not found in scan")
  }
  if(is.null(scan$geo$lat) && is.na(lat)) stop("radar latitude cannot be found in scan, specify using 'lat' argument")
  if(is.null(scan$geo$lon) && is.na(lon)) stop("radar longitude cannot be found in scan, specify using 'lon' argument")
  # check crs argument as in raster::raster()
  crs=CRS(as.character(projection(crs)))
  assert_that(is.number(k))
  assert_that(is.number(re))
  assert_that(is.number(rp))

  # georeference the data
  spdf=scan_to_spatial(scan, k = k, lat=lat, lon=lon, re=re, rp=rp)
  # keep only selected scan parameters
  if(!are_equal(param,NA)) spdf=spdf[param]
  # need to temporarily remove NA and NaN values, as raster::rasterize() does not accept these values
  NAN_TMP=99998
  NA_TMP=99999
  nan_idx=is.nan(spdf@data)
  spdf@data[is.na(spdf@data)]=NA_TMP  #note: this changes both NaN and NA
  spdf@data[nan_idx]=NAN_TMP
  # transform spatialpoints to coordinate system of the raster
  spdf=spTransform(spdf,crs)
  # get extent of the available data
  spdf_extent=raster::extent(spdf)
  # prepare a raster matching the data extent (or user-specified extent)
  if(are_equal(xlim,NA)) xlim=c(spdf_extent@xmin,spdf_extent@xmax)
  if(are_equal(ylim,NA)) ylim=c(spdf_extent@ymin,spdf_extent@ymax)
  if(is.na(res)){
    r <- raster(ncols=nx, nrows=ny,ext=raster::extent(c(xlim,ylim)),crs=crs)
  }
  else{
    r <- raster(ncols=nx, nrows=ny,ext=raster::extent(c(xlim,ylim)),crs=crs,res=res)
  }
  # fill the raster (this command takes up the bulk of the compute time)
  output=rasterize(spdf,r,field=names(spdf))
  # restore the NA and NaN values
  raster::values(output)[raster::values(output)==NAN_TMP]=NaN
  raster::values(output)[raster::values(output)==NA_TMP]=NA
  output
}


