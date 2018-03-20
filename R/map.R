#' Map a plan position indicator (ppi)
#'
#' Plot a ppi on a Google Maps, OpenStreetMap, Stamen Maps or Naver Map base layer map using \link[ggmap]{ggmap}
#' @param x an object of class 'ppi'
#' @param map the basemap to use, result of a call to \link{basemap}
#' @param param the scan parameter to plot
#' @param alpha transparency of the data, value between 0 and 1
#' @param radar.size size of the symbol indicating the radar position
#' @param radar.color colour of the symbol indicating the radar position
#' @param n.color the number of colors (>=1) to be in the palette
#' @param xlim range of x values to plot (degrees longitude), as atomic vector of length 2
#' @param ylim range of y values to plot (degrees latitude), as an atomic vector of length 2
#' @param zlim the range of values to plot
#' @param ratio aspect ratio between x and y scale, by default \eqn{1/cos(latitude radar * pi/180)}
#' @param ... arguments passed to low level \link[ggmap]{ggmap} function
#' @export
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @details
#' Available scan parameters for mapping can by printed to screen by \code{summary(x)}.
#' Commonly available parameters are:
#' \describe{
#'  \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'  \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity [m/s]. Radial velocities towards
#'   the radar are negative, while radial velocities away from the radar are positive}
#'  \item{"\code{RHOHV}"}{Correlation coefficient [unitless]. Correlation between vertically polarized and horizontally polarized reflectivity factor}
#'  \item{"\code{PHIDP}"}{Differential phase [degrees]}
#'  \item{"\code{ZDR}"}{(Logged) differential reflectivity [dB]}
#' }
#' The scan parameters
#' are named according to the OPERA data information model (ODIM), see
#' Table 16 in the \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
#' @examples
#' # load an example scan:
#' data(SCAN)
#' # make ppi's for all scan parameters in the scan
#' ppi=ppi(SCAN)
#' # grab a basemap that matches the extent of the ppi:
#' basemap=basemap(ppi)
#' # map the radial velocity scan parameter onto the basemap:
#' map(ppi,map=basemap,param="VRADH")
#' # extend the plotting range of velocities, from -50 to 50 m/s:
#' map(ppi,map=basemap,param="VRADH",zlim=c(-50,50))
#' # give the data less transparency:
#' map(ppi,map=basemap,alpha=0.9)
#' # change the appearance of the symbol indicating the radar location:
#' map(ppi,map=basemap,radar.size=5,radar.color="green")
#' # crop the map:
#' map(ppi,map=basemap,xlim=c(12.4,13.2),ylim=c(56,56.5))
map <- function (x, ...) UseMethod("map", x)

#' @describeIn map plot a 'ppi' object on a map
#' @export
map.ppi=function(x,map,param,alpha=0.7,xlim,ylim,zlim=c(-20,20),ratio,radar.size=3,radar.color="red",n.color=1000,...){
  stopifnot(inherits(x,"ppi"))
  if(missing(param)){
    if("DBZH" %in% names(x$data)) param="DBZH"
    else param=names(x$data)[1]
  }
  else if(!is.character(param)) stop("'param' should be a character string with a valid scan parameter name")
  if(missing(zlim)) zlim=get_zlim(param)
  if(!(param %in% names(x$data))) stop(paste("no scan parameter '",param,"' in this ppi",sep=""))
  if(!attributes(map)$ppi) stop("not a ppi map, use basemap() to download a map")
  if(attributes(map)$geo$lat!=x$geo$lat || attributes(map)$geo$lon!=x$geo$lon) stop("not a basemap for this radar location")
  # extract the scan parameter
  data=do.call(function(y) x$data[y],list(param))
  wgs84=CRS("+proj=longlat +datum=WGS84")
  epsg3857=CRS("+init=epsg:3857") # this is the google mercator projection
  mybbox=suppressWarnings(spTransform(SpatialPoints(t(data@bbox),proj4string=data@proj4string),CRS("+init=epsg:3857")))
  mybbox.wgs=suppressWarnings(spTransform(SpatialPoints(t(data@bbox),proj4string=data@proj4string),wgs84))
  e=raster::extent(mybbox.wgs)
  r <- raster(raster::extent(mybbox), ncol=data@grid@cells.dim[1]*.9, nrow=data@grid@cells.dim[2]*.9,crs=CRS(proj4string(mybbox)))
  # convert to google earth mercator projection
  data=suppressWarnings(as.data.frame(spTransform(data,CRS("+init=epsg:3857"))))
  # bring z-values within plotting range
  index=which(data$z<zlim[1])
  if(length(index)>0) data[index,]$z=zlim[1]
  index=which(data$z>zlim[2])
  if(length(index)>0) data[index,]$z=zlim[2]
  # rasterize
  r<-raster::rasterize(data[,2:3], r, data[,1])
  # assign colors
  if(param %in% c("VRADH","VRADV","VRAD")) cols=add.alpha(colorRampPalette(colors=c("blue","white","red"),alpha=TRUE)(n.color),alpha=alpha)
  else cols=add.alpha(colorRampPalette(colors=c("lightblue","darkblue","green","yellow","red","magenta"),alpha=TRUE)(n.color),alpha=alpha)

  col.func=function(value,lim){
    output=rep(0,length(value))
    output=round((value-lim[1])/(lim[2]-lim[1])*n.color)
    output[output>n.color]=n.color
    output[output<1]=1
    return(cols[output])
  }
  r@data@values=col.func(r@data@values,zlim)
  # these declarations prevent generation of NOTE "no visible binding for global variable" during package Check
  lon=lat=y=z=NA
  # symbols for the radar position
  # dummy is a hack to be able to include the ggplot2 color scale, radarpoint is the actual plotting of radar positions.
  dummy=geom_point(aes(x = lon, y = lat, colour=z),size=0,data=data.frame(lon=x$geo$lon,lat=x$geo$lat,z=0))
  radarpoint=geom_point(aes(x = lon, y = lat),colour=radar.color,size=radar.size,data=data.frame(lon=x$geo$lon,lat=x$geo$lat))
  # colorscale
  colorscale=get_colorscale(param,zlim)
  # bounding box
  bboxlatlon=attributes(map)$geo$bbox
  # remove dimnames, otherwise ggmap will give a warning message below:
  dimnames(bboxlatlon)=NULL
  if(missing(xlim)) xlim=bboxlatlon[1,]
  if(missing(ylim)) ylim=bboxlatlon[2,]
  # plot the data on the map
  mymap = suppressMessages(ggmap(map)+inset_raster(raster::as.matrix(r),e@xmin,e@xmax,e@ymin,e@ymax) + dummy + colorscale + radarpoint + scale_x_continuous(limits = xlim, expand = c(0, 0)) + scale_y_continuous(limits = ylim, expand = c(0, 0)))
  suppressWarnings(mymap)
}

#' Grab a basemap for a ppi
#'
#' downloads a Google Maps, OpenStreetMap, Stamen Maps or Naver Map base layer map using \link[ggmap]{get_map}
#' @param x an object of class 'ppi'
#' @param zoom zoom level (optional), see \link[ggmap]{get_map}. An integer from 3 (continent) to 21 (building).
#' By default the zoom level matching the ppi extent is selected automatically.
#' @param alpha transparancy of the basemap (0-1)
#' @param verbose logical. whether to print information to console
#' @param ... arguments to pass to \link[ggmap]{get_map} function
#' @export
#' @examples
#' # load an example scan:
#' data(SCAN)
#' # print summary info for the scan:
#' SCAN
#' # make ppi for the scan
#' ppi=ppi(SCAN)
#' # grab a basemap that matches the extent of the ppi:
#' basemap=basemap(ppi)
#' # map the reflectivity quantity of the ppi onto the basemap:
#' map(ppi,map=basemap,param="DBZH")
#' # download a different type of basemap, e.g. satellite imagery:
#' # see get_map() in ggmap library for full documentation of options
#' basemap=basemap(ppi,maptype="satellite")
#' # map the radial velocities onto the satellite imagery:
#' map(ppi,map=basemap,param="VRADH")
basemap=function(x,verbose=TRUE,zoom,alpha=1,...){
  stopifnot(inherits(x,"ppi"))
  if(!missing(zoom)) if(!is.numeric(zoom)) stop("zoom should be a numeric integer")
  # check size of ppi and determine zoom
  if(missing(zoom)) use_zoom=calc_zoom(x$geo$bbox["lon",],x$geo$bbox["lat",])
  else use_zoom=zoom
  if(verbose) cat("downloading zoom =",use_zoom,"...\n")
  map=get_map(location=c(lon=mean(x$geo$bbox["lon",]),lat=mean(x$geo$bbox["lat",])),zoom=use_zoom,...)
  bboxmap=attributes(map)$bb
  if((x$geo$bbox["lon","max"]-x$geo$bbox["lon","min"] > bboxmap$ur.lon - bboxmap$ll.lon) ||
     (x$geo$bbox["lat","max"]-x$geo$bbox["lat","min"] > bboxmap$ur.lat - bboxmap$ll.lat)){
    if(missing(zoom)){
      if(verbose) cat("map too small, downloading zoom =",use_zoom-1,"...\n")
      map=get_map(location=c(lon=mean(x$geo$bbox["lon",]),lat=mean(x$geo$bbox["lat",])),zoom=use_zoom-1,...)
      bboxmap=attributes(map)$bb
      if((x$geo$bbox["lon","max"]-x$geo$bbox["lon","min"] > bboxmap$ur.lon - bboxmap$ll.lon) ||
         (x$geo$bbox["lat","max"]-x$geo$bbox["lat","min"] > bboxmap$ur.lat - bboxmap$ll.lat)){
        if(verbose) cat("map still too small, downloading zoom =",use_zoom-2,"...\n")
        map=get_map(location=c(lon=mean(x$geo$bbox["lon",]),lat=mean(x$geo$bbox["lat",])),zoom=use_zoom-2,...)
      }
    } else warning("map is smaller than ppi bounding box")
  }
  attributes(map)$geo=x$geo
  attributes(map)$ppi=T
  # add transparency
  add.alpha(map, alpha=alpha)
}

get_zlim=function(param){
  if(param %in% c("DBZH","DBZV","DBZ")) return(c(-20,30))
  if(param %in% c("VRADH","VRADV","VRAD")) return(c(-20,20))
  if(param == "RHOHV") return(c(0.4,1))
  if(param == "ZDR") return(c(-5,8))
  if(param == "PHIDP") return(c(-200,200))
}
