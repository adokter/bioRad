# install HDF5 library for R
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
# load HDF5 library for R
library(rhdf5)
# other CRAN packages:
library(sp)
library(raster)
library("colorRamps")
library("fields")
library(rgdal)


#install.packages("h5")
#library(devtools)
#install_github("mannau/h5",args = "--configure-vars='LIBS=-L/opt/local/lib CPPFLAGS=-I/opt/local/include'")

# user functions:

# apparent earth radius accounting for standard diffraction of radar wave
EARTHRADIUS=6371*4/3
# beam height in kilometre:
BeamHeight=function(range,elev) sqrt(range^2+(EARTHRADIUS)^2+2*range*(EARTHRADIUS)*sin(elev))-EARTHRADIUS

lon2UTM <- function(lon) {
  (floor((lon + 180)/6) %% 60) + 1
}
wgs2utm<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
# wgs2proj defaults to Dutch RD coordinates
wgs2proj<-function(lon,lat,proj4string="+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs"){
  xy <- data.frame(x = lon, y = lat)
  coordinates(xy) <- c("x", "y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(proj4string))
  return(res)
}

Cartesian2Polar=function(coords,elev=0){
  range = sqrt(coords[,1]^2 + coords[,2]^2)/cos(elev)
  azim = (1.5*pi-atan2(coords[,2],coords[,1])) %% (2*pi)
  data.frame(range=range,azim=azim)
}

Polar2Index=function(coords.polar,rangebin=1, azimbin=2*pi/360){
  row=floor(1 + coords.polar$range/rangebin)
  col=rev(floor(1 + coords.polar$azim/azimbin))
  data.frame(row=row,col=col)
}

SafeSubset=function(data,indexx,indexy){
  datadim=dim(data)
  if(indexx<1 || indexx > datadim[1] || indexy<1 || indexy> datadim[2]) out=NA
  else out=data[indexx,indexy]
  out
} 

SamplePolar=function(scan.cart,scan.polar,elev=0,rangebin=1, azimbin=2*pi/360){
  index=Polar2Index(Cartesian2Polar(coordinates(scan.cart),elev=elev),rangebin=rangebin,azimbin=azimbin)
  data=mapply(function(x,y) SafeSubset(scan.polar,x,y),x=index$row,y=index$col)
  SpatialGridDataFrame(grid=scan.cart,data=data.frame(z=data))
}

r.points = c(1, 63, 82, 94, 146, 177, 192, 209, 256)
r.values = c(255, 255, 163, 255, 255, 81, 81, 0, 0)
g.points = c(1, 65, 80, 111, 143, 256)
g.values = c(255, 255, 163, 163, 0, 0)
b.points = c(1, 80, 97, 111, 128, 160, 207, 256)
b.values = c(255, 0, 0, 82, 0, 0, 255, 0)
plot.colors = rgb(c(200, approx(r.points, r.values, seq(1, 256, length.out = 255))$y), c(200, approx(g.points, g.values, seq(1, 256, length.out = 255))$y), c(200, approx(b.points, b.values, seq(1, 256, length.out = 255))$y), maxColorValue = 255)

proj.scan=function(fname,iscan,iquantity,bin=1,range=50,project=F,proj4string="+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",projunit=1){
  # read the scan from hdf5
  # BUG: h5read sometimes gives a larger array than stored in hdf5
  scan.polar=h5read(fname,sprintf("/dataset%d/data%d/data",iscan,iquantity))
  # read position radar
  lat=h5readAttributes(fname,"/where")$lat
  lon=h5readAttributes(fname,"/where")$lon
  # read the quantity name, gain and offset
  quantity.name=h5readAttributes(fname,sprintf("dataset%d/data%d/what",iscan,iquantity))$quantity
  gain=h5readAttributes(fname,sprintf("dataset%d/data%d/what",iscan,iquantity))$gain
  offset=h5readAttributes(fname,sprintf("dataset%d/data%d/what",iscan,iquantity))$offset
  nodata=h5readAttributes(fname,sprintf("dataset%d/data%d/what",iscan,iquantity))$nodata
  # read the range and azimuth resolution for the scan from hdf5
  r.scale=h5readAttributes(fname,sprintf("dataset%d/where",iscan))$rscale
  a.scale=360/h5readAttributes(fname,sprintf("dataset%d/where",iscan))$nrays
  #a.scale=360/(dim(scan.polar)[2])
  elev=h5readAttributes(fname,sprintf("dataset%d/where",iscan))$elangle
  dims.polar=c(r.scale/1000,a.scale*pi/180)
  names(dims.polar)<-c("range","azim")
  # make a Cartesian grid according to bin and range specification
  radarpos=wgs2proj(lon,lat,proj4string=proj4string)@coords[1,]
  scan.cart=SpatialGrid(grid=GridTopology(-c(range,range)/2,c(bin,bin),round(rep(range/bin,2))))
  scan.proj=SpatialGrid(grid=GridTopology(radarpos-(1000/projunit)*c(range,range)/2,(1000/projunit)*c(bin,bin),round(rep(range/bin,2))),proj4string=CRS(proj4string))
  # name the coordinates
  coordnames(scan.cart)<-c("x","y")
  # project polar data on cartesian grid
  if(project) use.elev=elev*pi/180 else use.elev=0
  scan.cart=SamplePolar(scan.cart,scan.polar,elev=use.elev,rangebin=dims.polar[1],azimbin=dims.polar[2])
  # apply offset and gain
  data.decoded=scan.cart@data$z
  data.decoded[which(data.decoded==nodata)]=NA
  data.decoded=(offset+gain*data.decoded)
  scan.proj=SpatialGridDataFrame(grid=scan.proj,data=data.frame(z=data.decoded))
  names(scan.proj@data)[1]=quantity.name
  return(scan.proj)
}

proj.knmiscan=function(fname,iscan=2,iquantity="DBZH",bin=1,range=50,project=F,proj4string="+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",projunit=1){
  # read the scan from hdf5
  # BUG: h5read sometimes gives a larger array than stored in hdf5
  if(iquantity=="DBZH"){
    quantitystring="scan_Z_data"
    quantity.name="DBZH"
    gain=0.5
    offset=-31.5
  }
  else{
    quantitystring="scan_V_data"
    quantity.name="VRAD"
    if(iscan==1){
      gain=0.0259843
      offset=-3.3
    }
    if(iscan>1 && iscan<6){
      gain=0.188976
      offset=-24  
    }
    if(iscan==6 || iscan==7){
      gain=0.251969
      offset=-32
    }
    if(iscan==8 || iscan==9){
      gain=0.314961
      offset=-40
    }
    if(iscan>9){
      gain=0.377953
      offset=-48
    }
  } 
  scan.polar=h5read(fname,sprintf("/scan%d/%s",iscan,quantitystring))
  # read position radar
  lon=h5readAttributes(fname,"/radar1")$radar_location[1]
  lat=h5readAttributes(fname,"/radar1")$radar_location[2]
  # read the quantity name, gain and offset
  nodata=0
  # read the range and azimuth resolution for the scan from hdf5
  r.scale=1000*h5readAttributes(fname,sprintf("scan%d",iscan))$scan_range_bin
  a.scale=h5readAttributes(fname,sprintf("scan%d",iscan))$scan_azim_bin
  elev=h5readAttributes(fname,sprintf("scan%d",iscan))$scan_elevation
  dims.polar=c(r.scale/1000,a.scale*pi/180)
  names(dims.polar)<-c("range","azim")
  # make a Cartesian grid according to bin and range specification
  radarpos=wgs2proj(lon,lat,proj4string=proj4string)@coords[1,]
  scan.cart=SpatialGrid(grid=GridTopology(-c(range,range)/2,c(bin,bin),round(rep(range/bin,2))))
  scan.proj=SpatialGrid(grid=GridTopology(radarpos-(1000/projunit)*c(range,range)/2,(1000/projunit)*c(bin,bin),round(rep(range/bin,2))),proj4string=CRS(proj4string))
  # name the coordinates
  coordnames(scan.cart)<-c("x","y")
  # project polar data on cartesian grid
  if(project) use.elev=elev*pi/180 else use.elev=0
  scan.cart=SamplePolar(scan.cart,scan.polar,elev=use.elev,rangebin=dims.polar[1],azimbin=dims.polar[2])
  # apply offset and gain
  data.decoded=scan.cart@data$z
  data.decoded[which(data.decoded==nodata)]=NA
  data.decoded=(offset+gain*data.decoded)
  scan.proj=SpatialGridDataFrame(grid=scan.proj,data=data.frame(z=data.decoded))
  names(scan.proj@data)[1]=quantity.name
  return(scan.proj)
}

plot.scan=function(fname,iscan,iquantity,bin=1,range=50,zlim=c(-20,30),plot.colors=plot.colors,project=F,proj4string="+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",projunit=1){
  # plot image
  elev=h5readAttributes(fname,sprintf("dataset%d/where",iscan))$elangle
  scan.proj=proj.scan(fname=fname,iscan=iscan,iquantity=iquantity,bin=bin,range=range,project=project,proj4string=proj4string,projunit=projunit)
  par(pty="s")
  image(scan.proj,col=plot.colors,main=paste(names(scan.proj@data),round(elev,1),"deg"),zlim=zlim,ylab="south-north",xlab="west-east",axes=T)
  image.plot(scan.proj,col=plot.colors,zlim=zlim,legend.only=T)
}

plot.knmiscan=function(fname,iscan=2,col=plot.colors,zlim=c(-20,30), ...){
  elev=h5readAttributes(fname,sprintf("scan%d",iscan))$scan_elevation
  scan.proj=proj.knmiscan(fname=fname, iscan=iscan, ...)
  par(pty="s")
  image(scan.proj,col=col,axes=T,main=paste(names(scan.proj@data),round(elev,1),"deg"),zlim=zlim,ylab="south-north",xlab="west-east")
  image.plot(scan.proj,col=plot.colors,zlim=zlim,legend.only=T)
}

gttest=function(x,y){
  if(!is.na(x) && is.na(y)) return(x)
  if(is.na(x) && !is.na(y)) return(y)
  if(is.na(x) && is.na(y)) return(NA)
  if(abs(x)>abs(y)) return(x) else return(y)
}

NLcomposite=function(fname1,fname2,iscan=2,iquantity="DBZH",range=300, ...){
  # plot image
  proj1=proj.knmiscan(fname=fname1,iscan=iscan,iquantity=iquantity,range=range, ...)
  proj2=proj.knmiscan(fname=fname2,iscan=iscan,iquantity=iquantity,range=range, ...)
  gridmin=c(min(proj1@bbox["x","min"],proj2@bbox["x","min"]),min(proj1@bbox["y","min"],proj2@bbox["y","min"]))
  gridmax=c(max(proj1@bbox["x","max"],proj2@bbox["x","max"]),max(proj1@bbox["y","max"],proj2@bbox["y","max"]))
  gridcellsize=proj1@grid@cellsize
  griddim=ceiling((gridmax-gridmin)/gridcellsize)
  scan.proj=SpatialGrid(grid=GridTopology(gridmin,gridcellsize,griddim),proj4string=proj1@proj4string)
  # name the coordinates
  coordnames(scan.proj)<-c("x","y")
  scan.proj=SpatialGridDataFrame(grid=scan.proj,data=data.frame(z=rep(NA,griddim[1]*griddim[2])))
  names(scan.proj@data)[1]=names(proj1@data)[1]
  out1=over(scan.proj,proj1)[,1]
  out2=over(scan.proj,proj2)[,1]
  if(iquantity=="DBZH"){
    scan.proj@data[,1]=pmax(out1,out2,na.rm=T)  
  }
  else{
    scan.proj@data[,1]=mapply(gttest,out1,out2)
  }
  return(scan.proj)
}

shape <- readOGR(dsn="/Users/adriaan/Documents/gis/NL/",layer="nederlandprov")
plotNLcomposite=function(fname1,fname2,iscan=2,iquantity="DBZH",zlim=if(iquantity=="DBZH") c(-20,10) else c(-25,25), ...){
  radarimage=NLcomposite(fname1,fname2,iscan=iscan,iquantity=iquantity, ...)
  if(iquantity=="DBZH"){
    cols=plot.colors
  }
  else{
    cols=matlab.like(255)
  } 
  spplot(stack(radarimage),col.regions=cols,cuts=255,zlim=zlim,sp.layout=shape,xlim=c(0,300000),ylim=c(300000,700000))
}

batchplot=function(fnames){
  for(fname in fnames){
    print(paste("processing",fname1,"..."))
    fname<<-fname
    fname1=fname
    fname2=sub("NL60","NL61",fname1)
    if(file.exists(fname1) && file.exists(fname2) && file.info(fname1)$size>0 && file.info(fname2)$size>0){
      pdf(sub(".h5","_dbzh.pdf",sub("RAD_NL60_VOL_NA_","",fname1)))
      print(plotNLcomposite(fname1,fname2,iquantity="DBZH"))
      dev.off()
      pdf(sub(".h5","_vrad.pdf",sub("RAD_NL60_VOL_NA_","",fname1)))
      print(plotNLcomposite(fname1,fname2,iquantity="VRAD"))
      dev.off()
      fileasc=sub(".h5","_dbzh.asc",sub("RAD_NL60_VOL_NA_","",fname1))
      writeRaster(unstack(stack(NLcomposite(fname1,fname2,iquantity="DBZH")))[[1]],filename=fileasc,format="ascii",overwrite=T)
      fileasc=sub(".h5","_vrad.asc",sub("RAD_NL60_VOL_NA_","",fname1))
      writeRaster(unstack(stack(NLcomposite(fname1,fname2,iquantity="VRAD")))[[1]],filename=fileasc,format="ascii",overwrite=T)
    }  
  }
}

setwd("~/Documents/radar/KNMI/201304/")
fnames=list.files(".","RAD_NL60_VOL*",recursive=T)
batchplot(fnames)

plotNLcomposite(fname1,fname2,iquantity="DBZH")
plotNLcomposite(fname1,fname2,iquantity="VRAD")
