#' Make a plan position indicator (ppi)
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
#' ppi=ppi(SCAN)
#' # print summary info for the ppi:
#' ppi
#' # copy the first scan parameter of the first scan in the volume to a new object 'param':
#' param=SCAN$params[[1]]
#' # make a ppi for the new 'param' object:
#' ppi=ppi(param)
#' # print summary info for this ppi:
#' ppi
ppi <- function (x,cellsize=500,range.max=50000,project=F,latlim=NULL,lonlim=NULL) UseMethod("ppi", x)

#' Subset `ppi`
#'
#' Extract by index from a ppi
#'
#' @param x an object of class 'param' or 'scan'
#' @param i indices specifying elements to extract
#'
#' @export
`[.ppi` <- function(x,i) {
  stopifnot(inherits(x,"ppi"))
  myppi=list(data=x$data[i],geo=x$geo)
  class(myppi)="ppi"
  return(myppi)
}

#' @describeIn ppi ppi for a single scan parameter
#' @export
ppi.param=function(x,cellsize=500,range.max=50000,project=F,latlim=NULL,lonlim=NULL){
  stopifnot(inherits(x,"param"))
  data=samplePolar(x,cellsize,range.max,project,latlim,lonlim)
  # copy the parameter's attributes
  geo=attributes(x)$geo
  geo$bbox=attributes(data)$bboxlatlon
  geo$merged=FALSE
  data=list(data=data, geo=geo)
  class(data)="ppi"
  data
}

#' @describeIn ppi multiple ppi's for all scan parameters in a scan
#' @export
ppi.scan=function(x,cellsize=500,range.max=50000,project=F,latlim=NULL,lonlim=NULL){
  stopifnot(inherits(x,"scan"))
  data=samplePolar(x$params[[1]],cellsize,range.max,project,latlim,lonlim)
  # copy the parameter's geo list to attributes
  geo=x$geo
  geo$bbox=attributes(data)$bboxlatlon
  geo$merged=FALSE
  if(length(x$params)>1){
    alldata=lapply(x$params,function(param) samplePolar(param,cellsize,range.max,project,latlim,lonlim))
    data=do.call(cbind,alldata)
  }
  data=list(data=data, geo=geo)
  class(data)="ppi"
  data
}

#' print method for ppi
#'
#' @param x An object of class \code{ppi}
#' @keywords internal
#' @export
print.ppi=function(x,digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "ppi"))
  cat("               Plan position indicator (class ppi)\n\n")
  cat("  quantities: ",names(x$data),"\n")
  cat("        dims: ",x$data@grid@cells.dim[1],"x",x$data@grid@cells.dim[2],"pixels\n\n")
}

#' Class 'ppi': plan position indicator
#' @param object object of class 'ppi'
#' @param x object of class 'ppi'
#' @param ... additional arguments affecting the summary produced.
#' @export
#' @method summary ppi
#' @details
#' A PPI of class 'ppi' is a list containing:
#' \describe{
#'  \item{\code{data}}{an object of class \link[sp]{SpatialGridDataFrame} containing the georeferenced data. Commonly available parameters are:
#'     \describe{
#'      \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'      \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity [m/s]. Radial velocities towards the radar are negative, while radial velocities away from the radar are positive}
#'      \item{"\code{RHOHV}"}{Correlation coefficient [unitless]. Correlation between vertically polarized and horizontally polarized reflectivity factor}
#'      \item{"\code{PHIDP}"}{Differential phase [degrees]}
#'      \item{"\code{ZDR}"}{(Logged) differential reflectivity [dB]}
#'        }
#'  }
#'  \item{\code{geo}}{geographic data, a list with:
#'     \describe{
#'      \item{\code{lat}}{latitude of the radar [decimal degrees]}
#'      \item{\code{lon}}{longitude of the radar [decimal degrees]}
#'      \item{\code{height}}{height of the radar antenna [metres above sea level]}
#'      \item{\code{elangle}}{radar beam elevation [degrees]}
#'      \item{\code{rscale}}{range bin size [m]}
#'      \item{\code{ascale}}{azimuth bin size [deg]}
#'     }
#'     The \code{geo} element of a 'scan' object is a copy of the \code{geo} element of its parent scan or scan parameter.
#'   }
#' }
summary.ppi=function(object, ...) print.ppi(object)

#' @rdname summary.ppi
#' @export
#' @return for \code{is.ppi}: \code{TRUE} if its argument is of class "\code{ppi}"
is.ppi <- function(x) inherits(x, "ppi")

#' @rdname summary.ppi
#' @export
#' @return for \code{dim.ppi}: dimensions of the ppi
dim.ppi <- function(x) {
  stopifnot(inherits(x,"ppi"))
  c(dim(x$data)[2],x$data@grid@cells.dim)
}


samplePolar=function(param,cellsize,range.max,project,latlim,lonlim){
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
    bbox=wgs2proj(bboxlatlon["lon",],bboxlatlon["lat",],proj4string)
    cellcentre.offset=c(min(bbox@coords[,"x"]),min(bbox@coords[,"y"]))
    cells.dim=c(ceiling((max(bbox@coords[,"x"])-min(bbox@coords[,"x"]))/cellsize),ceiling((max(bbox@coords[,"y"])-min(bbox@coords[,"y"]))/cellsize))
  }
  # define cartesian grid
  gridTopo=GridTopology(cellcentre.offset,c(cellsize,cellsize),cells.dim)
  # if projecting, account for elevation angle - not accounting for earths curvature
  if(project) elev=attributes(param)$geo$elangle*pi/180 else elev=0
  # get scan parameter indices, and extract data
  index=polar2index(cartesian2polar(coordinates(gridTopo),elev),attributes(param)$geo$rscale,attributes(param)$geo$ascale)
  data=data.frame(mapply(function(x,y) safeSubset(param,x,y),x=index$row,y=index$col))
  colnames(data)=attributes(param)$param
  output=SpatialGridDataFrame(grid=SpatialGrid(grid=gridTopo,proj4string=proj4string),data=data)
  attributes(output)$bboxlatlon=bboxlatlon
  output
}

# wgs2proj is a wrapper for spTransform
# proj4string should be an object of class 'CRS', as defined in package sp.
# returns an object of class SpatialPoints
wgs2proj<-function(lon,lat,proj4string){
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

cartesian2polar=function(coords,elev=0){
  range = sqrt(coords[,1]^2 + coords[,2]^2)/cos(elev)
  azim = (0.5*pi-atan2(coords[,2],coords[,1])) %% (2*pi)
  data.frame(range=range,azim=azim*180/pi)
}

safeSubset=function(data,indexx,indexy){
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
