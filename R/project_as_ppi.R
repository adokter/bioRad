#' Project a scan (\code{scan}) or parameter (\code{param}) to a plan position indicator (\code{ppi})
#'
#' Make a plan position indicator (ppi)
#' @param x an object of class 'param' or 'scan'
#' @param cellsize cartesian grid size in m
#' @param range.max maximum range in m
#' @param latlim the range of latitudes to include
#' @param lonlim the range of longitudes to include
#' @param project whether to vertically project onto earth's surface
#' @param ... arguments passed to methods
#' @export
#' @return an object of class '\link[=summary.ppi]{ppi}'.
#' @details The returned PPI is in Azimuthal Equidistant Projection.
#' @examples
#' # load a polar scan example object
#' data(SCAN)
#' SCAN
#' # make PPIs for all scan parameters in the scan:
#' ppi=project_as_ppi(SCAN)
#' # print summary info for the ppi:
#' ppi
#' # copy the first scan parameter of the first scan in the volume to a new object 'param':
#' param=SCAN$params[[1]]
#' # make a ppi for the new 'param' object:
#' ppi=project_as_ppi(param)
#' # print summary info for this ppi:
#' ppi
project_as_ppi <- function (x,cellsize=500,range.max=50000,project=F,latlim=NULL,lonlim=NULL) UseMethod("project_as_ppi", x)


#' @describeIn project_as_ppi ppi for a single scan parameter
#' @export
project_as_ppi.param=function(x,cellsize=500,range.max=50000,project=F,latlim=NULL,lonlim=NULL){
  stopifnot(inherits(x,"param"))
  data=sample_polar(x,cellsize,range.max,project,latlim,lonlim)
  # copy the parameter's attributes
  geo=attributes(x)$geo
  geo$bbox=attributes(data)$bboxlatlon
  geo$merged=FALSE
  data=list(radar=attributes(x)$radar,datetime=attributes(x)$datetime,data=data, geo=geo)
  class(data)="ppi"
  data
}

#' @describeIn project_as_ppi multiple ppi's for all scan parameters in a scan
#' @export
project_as_ppi.scan=function(x,cellsize=500,range.max=50000,project=F,latlim=NULL,lonlim=NULL){
  stopifnot(inherits(x,"scan"))
  data=sample_polar(x$params[[1]],cellsize,range.max,project,latlim,lonlim)
  # copy the parameter's geo list to attributes
  geo=x$geo
  geo$bbox=attributes(data)$bboxlatlon
  geo$merged=FALSE
  if(length(x$params)>1){
    alldata=lapply(x$params,function(param) sample_polar(param,cellsize,range.max,project,latlim,lonlim))
    data=do.call(cbind,alldata)
  }
  data=list(radar=x$radar,datetime=x$datetime,data=data, geo=geo)
  class(data)="ppi"
  data
}


sample_polar=function(param,cellsize,range.max,project,latlim,lonlim){
  #proj4string=CRS(paste("+proj=aeqd +lat_0=",attributes(param)$geo$lat," +lon_0=",attributes(param)$geo$lon," +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep=""))
  proj4string=CRS(paste("+proj=aeqd +lat_0=",attributes(param)$geo$lat," +lon_0=",attributes(param)$geo$lon," +units=m",sep=""))
  bboxlatlon=proj2wgs(c(-range.max,range.max),c(-range.max,range.max),proj4string)@bbox
  if(!missing(latlim) & !is.null(latlim)) bboxlatlon["lat",]=latlim
  if(!missing(lonlim) & !is.null(lonlim)) bboxlatlon["lon",]=lonlim
  if(missing(latlim) & missing(lonlim)){
    cellcentre.offset=-c(range.max,range.max)
    cells.dim=ceiling(rep(2*range.max/cellsize,2))
  }
  else{
    bbox=wgs_to_proj(bboxlatlon["lon",],bboxlatlon["lat",],proj4string)
    cellcentre.offset=c(min(bbox@coords[,"x"]),min(bbox@coords[,"y"]))
    cells.dim=c(ceiling((max(bbox@coords[,"x"])-min(bbox@coords[,"x"]))/cellsize),ceiling((max(bbox@coords[,"y"])-min(bbox@coords[,"y"]))/cellsize))
  }
  # define cartesian grid
  gridTopo=GridTopology(cellcentre.offset,c(cellsize,cellsize),cells.dim)
  # if projecting, account for elevation angle - not accounting for earths curvature
  if(project) elev=attributes(param)$geo$elangle*pi/180 else elev=0
  # get scan parameter indices, and extract data
  index=polar2index(cartesian_to_polar(coordinates(gridTopo),elev),attributes(param)$geo$rscale,attributes(param)$geo$ascale)
  data=data.frame(mapply(function(x,y) safe_subset(param,x,y),x=index$row,y=index$col))
  colnames(data)=attributes(param)$param
  output=SpatialGridDataFrame(grid=SpatialGrid(grid=gridTopo,proj4string=proj4string),data=data)
  attributes(output)$bboxlatlon=bboxlatlon
  output
}

# wgs_to_proj is a wrapper for spTransform
# proj4string should be an object of class 'CRS', as defined in package sp.
# returns an object of class SpatialPoints
wgs_to_proj<-function(lon,lat,proj4string){
  xy <- data.frame(x = lon, y = lat)
  coordinates(xy) <- c("x", "y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, proj4string)
  return(res)
}

# proj2wgs is a wrapper for spTransform
# proj4string should be an object of class 'CRS', as defined in package sp.
# returns an object of class SpatialPoints
proj2wgs<-function(x,y,proj4string){
  xy <- data.frame(lon=x, lat=y)
  coordinates(xy) <- c("lon", "lat")
  proj4string(xy) <- proj4string
  res <- spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
  return(res)
}

cartesian_to_polar=function(coords,elev=0){
  range = sqrt(coords[,1]^2 + coords[,2]^2)/cos(elev)
  azim = (0.5*pi-atan2(coords[,2],coords[,1])) %% (2*pi)
  data.frame(range=range,azim=azim*180/pi)
}

safe_subset=function(data,indexx,indexy){
  datadim=dim(data)
  if(indexx<1 || indexx > datadim[1] || indexy<1 || indexy> datadim[2]) out=NA
  else out=data[indexx,indexy]
  out
}

polar2index=function(coords.polar,rangebin=1, azimbin=1){
  row=floor(1 + coords.polar$range/rangebin)
  col=floor(1 + coords.polar$azim/azimbin)
  data.frame(row=row,col=col)
}

