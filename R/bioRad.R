#' Analyze and visualize biological signals in weather radar data
#'
#' \pkg{bioRad}
#' @details
#' \subsection{BioRad's class objects}{
#' \pkg{bioRad} uses the following class objects for storing radar data:
#'   \itemize{
#'     \item{\link[=summary.pvol]{pvol}}{, a polar volume: consists typically of a set of polar scans, collected at different elevation angles, that together sample the full aerial volume surrounding the radar}
#'     \item{\link[=summary.scan]{scan}}{, a polar scan: a 360 degree radar scan at a fixed elevation in polar coordinates. One scan typically contains multiple scan parameters.}
#'     \item{\link[=summary.param]{param}}{, a polar scan parameter: one of the observable quantities recorded within a polar scan, such as reflectivity (DBZH) or radial velocity (VRADH).}
#'     \item{\link[=summary.ppi]{ppi}}{, a Cartesian plan position indicator: a projection on a Cartesian grid of a polar scan or a polar scan parameter}
#'     \item{\link[=summary.vp]{vp}}{, a vertical profile: typically biological data extracted from a polar volume by \link{vol2bird}.}
#'     \item{\link[=summary.vplist]{vplist}}{, a list of \link[=summary.vp]{vp} objects.}
#'     \item{\link[=summary.vpts]{vpts}}{, a vertical profile time series: a time-oredered list of \link[=summary.vp]{vp} objects for a single radar.}
#'   }
#' }
#' The common \link[base]{summary}, \link[methods]{is}, \link[base]{dim}, and \link[base]{Extract} methods are available for each of these classes.
#'
#' Plot methods are available for ppi, vp and vpts objects
#' \subsection{Reading radar data}{
#' \pkg{bioRad} can read radar files in
#' \href{http://www.eumetnet.eu/sites/default/files/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM}
#' format, which is the implementation of the OPERA data information model in \href{https://support.hdfgroup.org/HDF5/}{HDF5} format,
#' or a format supported by the \href{http://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/}{RSL library}, such as NEXRAD data.
#' \pkg{bioRad}'s class objects are organised very similar to the OPERA data information model.
#'
#' Raw (level-II) weather radar data is read with the function \link{read.pvol}, which returns a \link[=summary.pvol]{pvol} polar volume object.
#'
#' Use the function \link{rsl2odim} to convert RSL (e.g. NEXRAD) radar data into ODIM HDF5 format.
#' }
#' \subsection{Mapping and projecting radar scans}{
#' Funtion \link{ppi} can be used to project polar scans or polar scan parameters onto a user-defined Cartesian grid
#'
#' Function \link{map} can be used together with \link{basemap} to overlay radar data with all kinds of publicly available map and satellite data.
#' }
#' \subsection{Processing weather radar data into vertical profiles of birds}{
#' \pkg{bioRad} contains an implementation of the \link{vol2bird} algorithm, which
#' processes polar volume data into a vertical profiles of birds (VPB).
#' \link{vol2bird} requires a locally running \href{https://www.docker.com/}{Docker} daemon.
#'
#' \link{vol2bird} outputs a vertical profile object (\link[=summary.vp]{vp}) in R,
#' and can store the vertical profile object in an ODIM-complient hdf5 format on disk.
#' Stored hdf5 profiles can be read from disk with \link{readvp} (for a single file) or with
#' \link{readvp.list} (for a list of files)
#'
#' For users running their own installation of vol2bird outside R and Docker, the function \link{readvp.table}
#' is provided to read vol2bird's stdout (standard output) into R (after piping the stdout to a file).
#' }
#' \subsection{Organizing, analyzing and plotting vertical profile data}{
#' Vertical profiles (\link[=summary.vp]{vp} objects) can be combined with \link[bioRad]{c.vp} into lists of vertical profiles (\link[=summary.vplist]{vplist} objects).
#'
#' Vertical profile lists (\link[=summary.vplist]{vplist} objects) can be converted into vertical profile time series (\link[=summary.vpts]{vpts} objects) using function \link{vpts}.
#'
#' \link{regularize} can be used to project a \link[=summary.vpts]{vpts} object on a regular time grid.
#' This is typically done before plotting a vertical profile time series.
#'
#' \link{plot.vp} can be used to make a plot of a single vertical profile.
#'
#' \link{plot.vpts} can be used to visually summarize vertical profile time series,
#' both in terms of density and speed, which can be visualized simultaneously (with colors and speed barbs).
#' }
#' \subsection{Conversions into numbers of migrating individuals}{
#' To convert radar reflectivity into densities of individuals, a specific radar
#' cross section per individual needs to be assumed, which is set with \link{rcs}.
#' By default, a radar cross section of 11 cm^2 is used, which is the average value found
#' by Dokter et al. during a full autumn migration season in Europe at C-band.
#'
#' \link{mtr} combines reflectivity and speed into migration traffic rates within user-defined altitude bands
#'
#' \link{mt} calculates migration traffic: it integrates migration traffic rates over time and altitude, to find the total number
#' of individuals passing a radar station in a certain time period
#'
#' \link{cmt} calculates cumulative migration traffic.
#' }
#' \subsection{Other useful functionality}{
#'  \itemize{
#'  \item \link{suntime} calculates runrise and sunset times
#'  \item \link{checkDocker} checks whether your local Docker daemon is running correctly
#'  \item \link{elangle} gives the elevation angle(s) of a polar volume or polar scan object
#'  \item \link{beamheight} gives the radar beam height, for a certain elevation and range.
#'  \item \link{beamwidth} gives the radar beam width, for a certain range.
#'  }
#' }
#' \subsection{Example datasets}{
#' \itemize{
#' \item \link{SCAN}: example object of class \link[=summary.scan]{scan}.
#' \item \link{VP}: example object of class \link[=summary.vp]{vp} as generated by \link{vol2bird}.
#' \item \link{VPTS}: example object of class \link[=summary.vpts]{vpts}.
#' \item \code{profile.h5}: example hdf5 file containing a vertical profile generated by \link{vol2bird}. Locate this file in your local installation with \code{system.file("extdata", "profile.h5", package="bioRad")}. Read it with \link{readvp}.
#' \item \code{volume.h5}: example hdf5 file containing a polar volume. Locate this file in your local installation with \code{system.file("extdata", "volume.h5", package="bioRad")}. Read it with \link{read.pvol}.
#' \item \code{VPTable.txt}: example standard output of \link{vol2bird} piped to a text file. Locate this file in your local installation with \code{system.file("extdata", "VPtable.txt", package="bioRad")}. Read it with \link{readvp.table}.
#' }
#' }
#'
#' @references
#' \itemize{
#'  \item Bird migration flight altitudes studied by a network of operational weather radars, Dokter et al., J. R. Soc. Interace 8 (54), pp. 30--43, 2011. DOI \href{http://dx.doi.org/10.1098/rsif.2010.0116}{10.1098/rsif.2010.0116}
#' }
#'
#' @import stats
#' @import rhdf5
#' @import fields
#' @import methods
#' @import graphics
#' @import ggplot2
#' @import ggmap
#' @import rgdal
#' @import sp
#' @import utils
#' @importFrom raster rasterToPoints
#' @importFrom raster raster
#'
"_PACKAGE"
#> [1] "_PACKAGE"


#' flag indicating whether docker is running
#' @keywords internal
docker=F
#' flag indicating whether vol2bird docker container is mounted
#' @keywords internal
mounted=F
#' @keywords internal
#' the current mountpoint of the vol2bird docker container
mount="~/"

#VP=readvp("~/git/bioRad/inst/extdata/profile.h5")
#save(VP,file="~/git/bioRad/data/VP.RData")
#' Example object of class \code{\link[=summary.vp]{vp}} as generated by \code{\link{vol2bird}} or \code{\link{readvp}}
#' @rdname vp-dataset
"VP"

#pvol <- system.file("extdata", "volume.h5", package="bioRad")
#vol=read.pvol(pvol)
#SCAN=vol$scans[[1]]
#save(SCAN,file="~/git/bioRad/data/SCAN.RData")
#' Example object of class \code{\link[=summary.scan]{scan}}
#' @rdname scan-dataset
"SCAN"

# locate example file:
# VPtable <- system.file("extdata", "VPtable.txt", package="bioRad")
# load time series:
# VPTS=readvp.table(VPtable,radar="KBGM", wavelength='S')
# rcs(VPTS)<-11
# save(VPTS,file="~/git/bioRad/data/VPTS.RData",compress="xz")
#' Example object of class \code{vpts}
#'
#' Example object of class \code{vpts}, a time series of vertical profiles
#' @rdname vpts-dataset
"VPTS"

#' Checks that Docker is running
#'
#' Checks that \href{https://www.docker.com/}{Docker} daemon is running correctly on the local system
#' @param verbose logical which indicates whether to print test results to R console. On Windows always TRUE.
#' @export
checkDocker = function(verbose=T){
  if(.Platform$OS.type=="unix"){
    system("docker rm -f hello-world",ignore.stderr=T,ignore.stdout=T)
    result=system("docker run --name hello-world hello-world",ignore.stderr=!verbose,ignore.stdout=!verbose)
  }
  else{
    system("docker rm -f hello-world",ignore.stderr=T,ignore.stdout=T,show.output.on.console=FALSE)
    result=system("docker run --name hello-world hello-world",ignore.stderr=!verbose,ignore.stdout=!verbose,show.output.on.console = TRUE)
  }
  parent.env=environment(checkDocker)
  unlockBinding("docker", parent.env)
  unlockBinding("mounted", parent.env)
  parent.env$docker=(result==0)
  parent.env$mounted=F
  lockBinding("docker", parent.env)
  lockBinding("mounted", parent.env)
  if(!verbose) return(result)
}

startContainer = function(mount="~/"){
  parent.env=environment(startContainer)
  # if docker not running, cannot start container
  if(!parent.env$docker) return(1)
  # if container already running at this mount point, nothing to be done:
  if(parent.env$mounted & parent.env$mount==mount) return(0)
  # remove any existing vol2bird containers
  if(.Platform$OS.type=="unix") system("docker rm -f vol2bird", ignore.stderr=T,ignore.stdout=T)
  else system("docker rm -f vol2bird", ignore.stderr=T,ignore.stdout=T,show.output.on.console = FALSE)
  # fire up the container:
  if(.Platform$OS.type=="unix") result=system(paste("docker run -v ",normalizePath(mount,winslash="/"),":/data -d --name vol2bird adokter/vol2bird sleep infinity",sep=""),ignore.stdout=T)
  else result=system(paste("docker run -v ",normalizePath(mount,winslash="/"),":/data -d --name vol2bird adokter/vol2bird sleep infinity",sep=""),ignore.stdout=T,show.output.on.console=FALSE)
  if(result!=0) warning(paste("failed to mount",mount,"... Go to 'Docker -> preferences -> File Sharing' and add this directory (or its root directory) as a bind mounted directory"))
  else{
    unlockBinding("mounted",parent.env)
    unlockBinding("mount",parent.env)
    parent.env$mounted=(result==0)
    parent.env$mount=mount
    lockBinding("mounted", parent.env)
    lockBinding("mount", parent.env)
  }
  return(result)
}

setLoadActions(function(ns)
  cat("Loading package", sQuote(getNamespaceName(ns)),"version",as.character(packageVersion(getNamespaceName(ns))),"...\n"),
  function(ns) if((checkDocker(verbose=F)!=0)){
    cat("Warning: no running Docker daemon found\n")
    cat("Warning:",getNamespaceName(ns),"functionality requiring Docker has been disabled\n\n")
    cat("To enable Docker functionality, start Docker and run 'checkDocker()' in R")
    unlockBinding("docker", environment(checkDocker))
    assign("docker", F, envir = ns)
  }
  else{
    cat("Docker daemon running, Docker functionality enabled.\n")
  }
)

readOdimProfileData = function(file,group){
  whatgroup=h5readAttributes(file,sprintf("%s/what",group))
  nodata=whatgroup$nodata
  undetect=whatgroup$undetect
  gain=whatgroup$gain
  offset=whatgroup$offset
  data=h5read(file,sprintf("%s/data",group))[1,]
  data=replace(data,data==nodata,NA)
  data=replace(data,data==undetect,NaN)
  offset+gain*data
}

quantityName = function(file,group){
  whatgroup=h5readAttributes(file,paste(group,"/what",sep=""))
  whatgroup$quantity
}

#' Read a vertical profile (vp) from file
#'
#' @param filename A string containing the path to a vertical profile generated by \link[bioRad]{vol2bird}
#' @export
#' @return an object of class \link[bioRad]{summary.vp}
#' @examples
#' # locate example profile file:
#' prof <- system.file("extdata", "profile.h5", package="bioRad")
#' # print the local path of the profile file:
#' prof
#' # load the file:
#' readvp(prof)
#'
readvp = function(filename){
  if(!is.vpfile(filename)) return(NULL)
  #check input argument
  groups=h5ls(filename,recursive=F)$name
  if(!("dataset1" %in% groups)){
    stop("HDF5 file does not contain a /dataset1 group")
  }
  #extract quantities
  groups=h5ls(filename)
  groups=groups[which(groups$name=="data"),]$group
  quantities=sapply(groups,function(x) quantityName(filename,x))
  profile=as.data.frame(lapply(groups,function(x) readOdimProfileData(filename,x)))
  names(profile)=quantities

  #extract attributes
  attribs.how=h5readAttributes(filename,"how")
  attribs.what=h5readAttributes(filename,"what")
  attribs.where=h5readAttributes(filename,"where")

  #convert some useful metadata
  datetime=as.POSIXct(paste(attribs.what$date, attribs.what$time), format = "%Y%m%d %H%M%S", tz='UTC')
  sources=strsplit(attribs.what$source,",")[[1]]
  radar=gsub("RAD:","",sources[which(grepl("RAD:",sources))])
  filename=basename(filename)

  #prepare output
  output=list(radar=radar,datetime=datetime,filename=filename,data=profile,attributes=list(how=attribs.how,what=attribs.what,where=attribs.where))
  class(output) = "vp"
  output
}

#' Check whether file is a vertical profile
#'
#' Checker whether a file is a vertical profile that can be read with package \pkg{bioRad}
#'
#' @param filename A string containing a filename
#' @export
#' @return TRUE when \code{filename} is a vertical profile, otherwise FALSE
#' @examples
#' profile <- system.file("extdata", "profile.h5", package="bioRad")
#' is.vpfile(profile)   #> TRUE
#'
is.vpfile = function(filename){
  type=h5ODIMobject(filename)
  if(is.na(type)) return(FALSE)
  else return(type=="VP")
}

#' Check whether file is a polar volume
#'
#' Checker whether a file is a polar volume that can be read with package \pkg{bioRad}
#'
#' @param filename A string containing a filename
#' @export
#' @return TRUE when \code{filename} is a polar volume in readable format, otherwise FALSE
#' @examples
#' volume <- system.file("extdata", "volume.h5", package="bioRad")
#' is.pvolfile(volume)   #> TRUE
#'
is.pvolfile = function(filename){
  type=h5ODIMobject(filename)
  if(is.na(type)) return(FALSE)
  else return(type=="PVOL")
}

#' Check ODIM HDF5 data class
#'
#' Checks which data class is contained in ODIM HDF5 file
#'
#' @param filename A string containing a filename
#' @export
#' @return character string "\code{pvol}" for polar volume, "\code{vp}" for vertical profile, otherwise \code{NA}
#' @examples
#' # locate a polar volume file
#' pvol <- system.file("extdata", "volume.h5", package="bioRad")
#' h5ODIMobject(pvol)   #> "pvol"
#'
h5ODIMobject = function(filename){
  if(!file.exists(filename)){
    warning(paste(filename,"does not exist"))
    return(NA)
  }
  if(!is.ODIMfile(filename)){
    warning(paste(filename,"is not a ODIM HDF5 file"))
    return(NA)
  }
  object=h5readAttributes(filename,"what")$object
  return(object)
}

is.ODIMfile = function(filename){
  if(!H5Fis_hdf5(filename)){
    warning(paste(filename,"is not a HDF5 file"))
    return(FALSE)
  }
  output = T
  groups=h5ls(filename,recursive=F)$name
  if(!("dataset1" %in% groups)){
    output = F
    warning(paste("HDF5 file",filename,"does not contain a /dataset1 group"))
  }
  if(!("what" %in% groups)){
    output = F
    warning(paste("HDF5 file",filename,"does not contain a /what group"))
  }
  else{
    object=h5readAttributes(filename,"what")$object
    if(is.null(object)){
      warning("'object' attribute not found in /what group")
      output=F
    }
  }
  if(!("how" %in% groups)){
    output = F
    warning(paste("HDF5 file",filename,"does not contain a /how group"))
  }
  if(!("where" %in% groups)){
    output = F
    warning(paste("HDF5 file",filename,"does not contain a /where group"))
  }
  return(output)
}


#' print method for class \code{vp}
#'
#' @param x An object of class \code{vp}, like the result of a call to \link[=summary.vp]{readvp}
#' @keywords internal
#' @export
print.vp=function(x,digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "vp"))
  cat("               Vertical profile (class vp)\n\n")
  cat("       radar: ",x$radar,"\n")
  cat("      source: ",x$attributes$what$source,"\n")
  cat("nominal time: ",as.character(x$datetime),"\n")
  cat("generated by: ",paste(x$attributes$how$task,x$attributes$how$task_version),"\n")
}

#' Plot a vertical profile
#'
#' @param x a vp class object
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param line.col Color of the plotted curve
#' @param line.lwd Line width of the plotted curve
#' @param main an overall title for the plot
#' @param ... Additional arguments to be passed to the low level \link[graphics]{plot} plotting function
#' @export
#' @method plot vp
#' @examples
#' data(VP)
#' plot(VP)
#' plot(VP,line.col='blue')
plot.vp=function(x, xlab="density [#/km^3]",ylab="height [km]",main="Vertical profile",line.col='red',line.lwd=1,...){
  stopifnot(inherits(x,"vp"))
  pdat=x$data$dens
  pdat[is.na(pdat)]=0
  plot(pdat,x$data$HGHT/1000,xlab=xlab,ylab=ylab,main=main,...)
  points(pdat,x$data$HGHT/1000, col=line.col,lwd=line.lwd,type="l")
}

#' Read a list of vertical profiles from multiple files
#'
#' @param files A string vector containing the filenames of vertical profiles in ODIM HDF5 format generated by \link[bioRad]{vol2bird}
#' @export
#' @return an object of class \code{vplist}, which is a list \code{vp} objects
#' @examples
#' \dontrun{readvp(c("my/path/profile1.h5","my/path/profile2.h5", ...))}
#'
readvp.list=function(files){
  vps=lapply(files,readvp)
  # remove nulls
  vps <- vps[!sapply(vps, is.null)]
  do.call(c.vp,vps)
}


#' concatenate \code{vp} objects into a \code{vplist} object
#' @param ... objects of class \code{vp}
#' @export
#' @keywords internal
#' @return an object of class \code{vplist}, see \link[bioRad]{readvp.list} for details
c.vp = function(...){
  vps=list(...)
  vptest=sapply(vps,function(x) is(x,"vp"))
  if(FALSE %in% vptest) {
    warning("non-vp objects found, returning a standard list...")
    return(vps)
  }
  # extract radar identifiers
  radars=unique(sapply(vps,'[[',"radar"))
  if(length(radars)>1) warning("Vertical profiles are not from a single radar")
  # extract date-times
  dates=.POSIXct(do.call("c",lapply(vps,'[[',"datetime")),tz="UTC")
  output=list(vplist=vps,radar=radars,daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC"),dates=dates)
  output=vps
  class(output)="vplist"
  output
}

#' Class 'vplist': list of vertical profiles
#'
#' Class for list of vertical profiles
#' @param object object of class 'vplist'
#' @param x object of class 'vplist'
#' @param ... additional arguments affecting the summary produced.
#' @export
#' @method summary vplist
#' @details
#' details to be written
summary.vplist=function(object, ...) print.vplist(object)

#' @rdname summary.vplist
#' @param i indices specifying elements to extract
#' @export
`[.vplist` <- function(x,i) {
  stopifnot(inherits(x,"vplist"))
  if(length(i)==1) return(x[[i]])
  output=unclass(x)[i]
  class(output)="vplist"
  return(output)
}

#' print method for class \code{vplist}
#'
#' @param x An object of class \code{vplist}, usually a result of a call to \link[bioRad]{readvp.list}
#' @keywords internal
#' @export
print.vplist=function(x,digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "vplist"))
  # extract radar identifiers
  radar=unique(sapply(x,'[[',"radar"))
  # extract date-times
  dates=.POSIXct(do.call("c",lapply(x,'[[',"datetime")),tz="UTC")
  daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC")
  cat("                   List of vertical profiles (class vplist)\n\n")
  cat("          radars: ",radar,"\n")
  cat("      # profiles: ",length(x),"\n")
  cat("time range (UTC): ",as.character(daterange[1]),"-",as.character(daterange[2]),"\n")
}

#' Convert \code{vplist} to a single radar time series \code{vpts}
#'
#' @param x An object of class \code{vplist}, usually a result of a call to \link[bioRad]{readvp.list}
#' @param radar string containing the radar identifier to generate time series for. Only required when \code{vplist} object contains multiple radars
#' @export
#' @return an object of class \code{vpts}, which is a list containing
#' \describe{
#'  \item{\code{radar}}{string containing the radar identifier}
#'  \item{\code{dates}}{the \code{N} nominal times of the profiles}
#'  \item{\code{heights}}{the \code{M} heights of the layers in the profile}
#'  \item{\code{daterange}}{the minimum and maximum nominal time of the profiles in the list}
#'  \item{\code{timesteps}}{time differences between the profiles. Element \code{i} gives the time difference between profile \code{i} and \code{i+1}}
#'  \item{\code{data}}{list of \code{N} by \code{M} matrices containing the vertical profiles for each quantity.
#'                     For a description of available quantities, see the \code{data} element of the \code{vp} class in \link[=summary.vp]{readvp}}
#'  \item{\code{attributes}}{profile attributes, copied from the first profile contained in \code{x}}
#'  \item{\code{regular}}{logical indicating whether the time series is regular or not}
#' }
#' @rdname vpts
#' @examples
#' \dontrun{readvp(c("my/path/profile1.h5","my/path/profile2.h5", ...))}
#'
vpts=function(x,radar=NA){
  stopifnot(inherits(x, "vplist"))
  # extract radar identifiers
  radars=sapply(x,'[[',"radar")
  uniqueRadars=unique(radars)
  # extract date-times
  dates=.POSIXct(do.call("c",lapply(x,'[[',"datetime")),tz="UTC")
  daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC")
  if(length(uniqueRadars)>1 & is.na(radar)) stop("vertical profile list of multiple radars, select one with 'radar' argument")
  if(!is.na(radar) & !(radar %in% uniqueRadars)) stop(paste("no profiles found for radar",radar))
  if(is.na(radar) & length(uniqueRadars==1)) radar=uniqueRadars
  index=which(radars==radar)
  vps=x[index]
  # sort by datetime
  vps=vps[order(sapply(vps,'[[',"datetime"))]
  dates=.POSIXct(do.call("c",lapply(vps,'[[',"datetime")),tz="UTC")
  difftimes=difftime(dates[-1],dates[-length(dates)],units="secs")
  profile.quantities=names(vps[[1]]$data)

  where.attributes=sapply(lapply(vps,'[[',"attributes"),'[[',"where")
  if(length(unique(unlist(where.attributes["interval",])))>1) stop("vertical profiles have different altitude bin size")
  if(length(unique(unlist(where.attributes["levels",])))>1) stop("vertical profiles have different number of altitude bins")
  if(length(unique(unlist(where.attributes["maxheight",])))>1) stop("vertical profiles have different maxheight")
  if(length(unique(unlist(where.attributes["minheight",])))>1) stop("vertical profiles have different minheight")
  vpsFlat=lapply(profile.quantities, function(quantity) sapply(lapply(vps,'[[',"data"),'[[',quantity))
  names(vpsFlat)=profile.quantities
  if(length(unique(difftimes))==1) regular = T else regular = F
  vpsFlat$HGHT<-NULL
  output=list(radar=radar,dates=dates,heights=vps[[1]]$data$HGHT,daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC"),timesteps=difftimes,data=vpsFlat,attributes=vps[[1]]$attributes,regular=regular)
  class(output)="vpts"
  output
}

#' print method for class \code{vpts}
#'
#' @param x An object of class \code{vpts}, usually a result of a call to \link[bioRad]{vpts}
#' @keywords internal
#' @export
print.vpts=function(x,digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "vpts"))
  cat("                  ",if(x$regular) "Regular" else "Irregular","time series of vertical profiles (class vpts)\n\n")
  cat("           radar: ",x$radar,"\n")
  cat("      # profiles: ",length(x$dates),"\n")
  cat("time range (UTC): ",as.character(x$daterange[1]),"-",as.character(x$daterange[2]),"\n")
  if(x$regular) cat("   time step (s): ",min(x$timesteps),"\n")
  else cat("   time step (s): ","min:",min(x$timesteps),"    max: ",max(x$timesteps),"\n")
}

#' Calculate a vertical profile of birds (VPB)
#'
#' Calculates a vertical profile of birds (VPB) from a polar volume
#' @param vol.in A radar file containing a radar polar volume, either in
#' \href{http://www.eumetnet.eu/sites/default/files/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM}
#' format, which is the implementation of the OPERA data information model in \href{https://support.hdfgroup.org/HDF5/}{HDF5} format,
#' or a format supported by the \href{http://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/}{RSL library}.
#' @param vp.out character string. Filename for the vertical profile to be generated in ODIM HDF5 format (optional)
#' @param vol.out character string. Filename for the polar volume to be generated in ODIM HDF5 format (optional, e.g. for converting RSL formats to ODIM)
#' @param verbose logical. When TRUE, pipe Docker stdout to R console. On Windows always TRUE
#' @param mount character string with the mount point (a directory path) for the Docker container
#' @param sd_vvp numeric. lower threshold in radial velocity standard deviation (\code{sd_vvp}) in m/s.
#' @param rcs numeric. Radar cross section per bird in cm^2.
#' @param dualpol logical. When \code{TRUE} use dual-pol mode, in which meteorological echoes are filtered using the correlation coeficient \code{rhohv}.
#' When \code{FALSE} use single polarizaiton mode based only on reflectivity and radial velocity quantities.
#' @param rhohv numeric. Lower threshold in correlation coefficient used to filter meteorological scattering
#' @param elev.min numeric. Minimum scan elevation in degrees
#' @param elev.max numeric. Maximum scan elevation in degrees
#' @param azim.min numeric. Minimum azimuth in degrees clockwise from north
#' @param azim.max numeric. Maximum azimuth in degrees clockwise from north
#' @param range.min numeric. Minimum range in km
#' @param range.max numeric. Maximum range in km.
#' @param nlayer numeric. Number of altitude layers in the profile
#' @param hlayer numeric. Width of altitude layers in metre
#' @param nyquist.min numeric. Minimum Nyquist velocity of scans in m/s for scans to be included in the analysis
#' @param dealias logical. Whether to dealias radial velocities; this should typically be done
#'  when the scans in the polar volume have low Nyquist velocities (below 25 m/s)
#' @details Requires a running \href{https://www.docker.com/}{Docker} daemon
#'
#' Common arguments set by users are \code{vol.in}, \code{vp.out}, \code{dualpol}, \code{dealias} and \code{mount}
#'
#' Arguments that sometimes require non-default values are: \code{rcs}, \code{sd_vvp}, \code{range.max}
#'
#' Other arguments are typically left at their defaults.
#'
#' \code{azim.min} and \code{azim.max} only affects reflectivity-derived estimates in the profile (DBZH,eta,dens),
#' not radial-velocity derived estimates (u,v,w,ff,dd,sd_vvp), which are estimated on all azimuths at all times.
#' \code{azim.min},\code{azim.max} may be set to exclude an angular sector with high ground clutter
#'
#' \code{range.max} may be extended up to 40,000 m for volumes with low elevations only, in order to extend coverage to higher altitudes.
#'
#' For altitude layers with a VVP-retrieved radial velocity standard deviation value
#' below the threshold \code{sd_vvp}, the bird density \code{dens} is set to zero (see vertical profile \link[=summary.vp]{vp} class). This threshold might be
#' dependent on radar processing settings. Results from validation campaigns so far indicate that 2 m/s is
#' the best choice for this parameter for most weather radars.

#' The algorithm has been tested and developed for altitude layers with \code{hlayer} = 200 m.
#' Smaller widths are not recommended as they may cause instabilities of the volume velocity profiling (VVP)
#' and dealiasing routines, and effectively lead to pseudo-replicated altitude data, since altitudinal patterns
#' smaller than the beam width cannot be resolved.
#'
#' The default radar cross section (11 cm^2) corresponds to the average value found by Dokter et al.
#' in a calibration campaign of a full migration autumn season in western Europe at C-band.
#' It's value may depend on radar wavelength. rcs will scale approximately \eqn{M^{2/3}} with \code{M} the bird's mass.
#'
#' Using default values of \code{range.min} and \code{range.max} is recommended.
#' Ranges closer than 5 km tend to be contaminated by ground clutter, while range gates beyond
#' 25 km become too wide to resolve the default altitude layer width of 200 metre (see \link[bioRad]{beamwidth})
#'
#' For dealiasing, the torus mapping method by Haase et al. is used
#'
#' At S-band (radar wavelength ~ 10 cm), currently only \code{dualpol=T} mode is recommended.
#'
#' On repeated calls of \code{vol2bird}, the Docker container mount can be recycled
#' from one call to the next if subsequent calls share the same \code{mount} argument.
#' Re-mounting a Docker container takes time, therefore it is advised to
#' choose a mountpoint that is a parent directory of all volume files to be processed,
#' such that \code{vol2bird} calls are as fast as possible.
#' @export
#' @return A vertical profile object of class \link[=summary.vp]{vp}.
#' When defined, output files \code{vp.out} and \code{vol.out} are saved to disk.
#' @references
#' \itemize{
#'   \item Haase, G. and Landelius, T., 2004. Dealiasing of Doppler radar velocities using a torus mapping. Journal of Atmospheric and Oceanic Technology, 21(10), pp.1566-1573.
#'   \item Bird migration flight altitudes studied by a network of operational weather radars, Dokter et al., J. R. Soc. Interace 8 (54), pp. 30--43, 2011. DOI \href{http://dx.doi.org/10.1098/rsif.2010.0116}{10.1098/rsif.2010.0116}
#' }
#' @examples
#' # locate example volume file:
#' volume <- system.file("extdata", "volume.h5", package="bioRad")
#' # copy to a home directory with read/write permissions:
#' file.copy(volume,"~/volume.h5")
#' # calculate the profile:
#' \dontrun{profile=vol2bird("~/volume.h5")}
#' # clean up:
#' file.remove("~/volume.h5")
vol2bird =  function(vol.in, vp.out="", vol.out="",verbose=F,mount=dirname(vol.in),sd_vvp=2,rcs=11,dualpol=F,rhohv=0.9,elev.min=0,elev.max=90,azim.min=0,azim.max=360,range.min=5000,range.max=25000,nlayer=20L,hlayer=200,dealias=T,nyquist.min=if(dealias) 5 else 25){
  # check input arguments
  if(!is.numeric(sd_vvp) || sd_vvp<=0) stop("invalid 'sd_vvp' argument, radial velocity standard deviation threshold should be a positive numeric value")
  if(!is.numeric(rcs) || rcs<=0) stop("invalid 'rcs' argument, radar cross section should be a positive numeric value")
  if(!is.logical(dualpol)) stop("invalid 'dualpol' argument, should be logical")
  if(!is.numeric(rhohv) || rhohv<=0 || rhohv>1) stop("invalid 'rhohv' argument, correlation coefficient treshold should be a numeric value between 0 and 1")
  if(!is.numeric(elev.min) || elev.min< -90 || elev.min>90) stop("invalid 'elev.min' argument, elevation should be between -90 and 90 degrees")
  if(!is.numeric(elev.max) || elev.max< -90 || elev.max>90) stop("invalid 'elev.max' argument, elevation should be between -90 and 90 degrees")
  if(elev.max<elev.min) stop("'elev.max' cannot be larger than 'elev.min'")
  if(!is.numeric(azim.min) || azim.min<0 || azim.min>360) stop("invalid 'azim.min' argument, azimuth should be between 0 and 360 degrees")
  if(!is.numeric(azim.max) || azim.max<0 || azim.max>360) stop("invalid 'azim.max' argument, azimuth should be between 0 and 360 degrees")
  if(!is.numeric(range.min) || range.min<0) stop("invalid 'range.min' argument, range should be a positive numeric value")
  if(!is.numeric(range.max) || range.max<0) stop("invalid 'range.max' argument, range should be a positive numeric value")
  if(range.max<range.min) stop("'rang.max' cannot be larger than 'rang.min'")
  if(!is.integer(nlayer) & nlayer<=0) stop("'nlayer' should be a positive integer")
  if(!is.numeric(hlayer) || hlayer<0) stop("invalid 'hlayer' argument, should be a positive numeric value")
  if(!is.numeric(nyquist.min) || nyquist.min<0) stop("invalid 'nyquist.min' argument, should be a positive numeric value")
  if(!is.logical(dealias)) stop("invalid 'dealias' argument, should be logical")
  if(file.access(mount,0)==-1) stop("invalid 'mount' argument. Directory not found")
  if(file.access(mount,2)==-1) stop(paste("invalid 'mount' argument. No write permission in directory",mount))
  if(!docker) stop("Requires a running Docker daemon.\nTo enable vol2bird, start your local Docker daemon, and run 'checkDocker()' in R\n")
  if(!file.exists(vol.in)) stop("No such file or directory")
  if(!length(verbose)==1 || !is.logical(verbose)) stop("verbose argument should be one of TRUE or FALSE")
  if(vp.out!="" && !file.exists(dirname(vp.out))) stop(paste("output directory",dirname(vp.out),"not found"))
  filedir=dirname(normalizePath(vol.in,winslash="/"))
  if(!grepl(normalizePath(mount,winslash="/"),filedir,fixed=T)) stop("mountpoint 'mount' has to be a parent directory of input file 'vol.in'")
  profile.tmp=tempfile(tmpdir=filedir)
  if(file.access(filedir,mode=2)<0) stop(paste("vol2bird requires write permission in",filedir))
  if(startContainer(normalizePath(mount,winslash="/"))!=0) stop(paste("failed to start vol2bird Docker container"))

  # put options file in place, to be read by vol2bird container
  opt.values=c(as.character(c(sd_vvp,rcs,rhohv,elev.min,elev.max,azim.min,azim.max,range.min,
                 range.max,nlayer,hlayer,nyquist.min)),
                 if(dualpol) "TRUE" else "FALSE",if(dealias) "TRUE" else "FALSE")
  opt.names=c("STDEV_BIRD","SIGMA_BIRD","RHOHVMIN","ELEVMIN","ELEVMAX",
                  "AZIMMIN","AZIMMAX","RANGEMIN","RANGEMAX","NLAYER","HLAYER",
                  "MIN_NYQUIST_VELOCITY","DEALIAS_VRAD","DUALPOL")
  opt=data.frame("option"=opt.names,"is"=rep("=",length(opt.values)),"value"=opt.values)
  optfile=paste(normalizePath(mount,winslash="/"),"/options.conf",sep="")
  if(file.exists(optfile)){
    warning(paste("options.conf file found in directory ",mount,". Renamed to options.conf.save to prevent overwrite...", sep=""))
    file.rename(optfile,paste(optfile,".saved",sep=""))
  }
  write.table(opt,file=optfile,col.names=F,row.names=F,quote=F)

  # prepare docker input filenames relative to mountpoint
  prefixstart=if(mount=="/") 1 else 2
  prefix=substring(filedir,prefixstart+nchar(normalizePath(mount,winslash="/")))
  if(nchar(prefix)>0) prefix=paste(prefix,"/",sep="")
  vol.in.docker=paste(prefix,basename(vol.in),sep="")
  profile.tmp.docker=paste(prefix,basename(profile.tmp),sep="")
  if(vol.out!="") vol.out.docker=paste(prefix,basename(vol.out),sep="")
  else vol.out.docker=""

  # run vol2bird container
  if(.Platform$OS.type=="unix") result = system(paste("docker exec vol2bird bash -c \"cd data && vol2bird ",vol.in.docker,profile.tmp.docker,vol.out.docker,"\""),ignore.stdout=!verbose)
  else{
    winstring=paste("docker exec vol2bird bash -c \"cd data && vol2bird ",vol.in.docker,profile.tmp.docker,vol.out.docker,"\"")
    result = system(winstring)
  }
  if(result!=0){
    file.remove(optfile)
    stop("failed to run vol2bird Docker container")
  }

  # read output into a vp object
  output=readvp(profile.tmp)

  # clean up
  if(vp.out=="") file.remove(profile.tmp)
  else file.rename(profile.tmp,vp.out)
  file.remove(optfile)

  output
}

#' convert RSL polar volume to ODIM hdf5 format
#' @param vol.in polar volume input file in RSL format
#' @param vol.out filename for the polar volume in ODIM hdf5 format to be generated
#' @inheritParams vol2bird
#' @export
#' @return \code{TRUE} on success
rsl2odim =function(vol.in,vol.out,verbose=F,mount=dirname(vol.in)){
  if(!file.exists(dirname(vol.out))) stop(paste("output directory",dirname(vol.out),"not found"))
  if(file.access(dirname(vol.out),2)==-1) stop(paste("No write permission in directory",dirname(vol.out)))
  vol.tmp=rsl2odim_tempfile(vol.in,verbose,mount)
  file.rename(vol.tmp,vol.out)
}

rsl2odim_tempfile =  function(vol.in,verbose=F,mount=dirname(vol.in)){
  # check input arguments
  if(file.access(mount,0)==-1) stop("invalid 'mount' argument. Directory not found")
  if(file.access(mount,2)==-1) stop(paste("invalid 'mount' argument. No write permission in directory",mount))
  if(!docker) stop("Requires a running Docker daemon.\nTo enable, start your local Docker daemon, and run 'checkDocker()' in R\n")
  if(!file.exists(vol.in)) stop("No such file or directory")
  if(!length(verbose)==1 || !is.logical(verbose)) stop("verbose argument should be one of TRUE or FALSE")
  filedir=dirname(normalizePath(vol.in,winslash="/"))
  if(!grepl(normalizePath(mount,winslash="/"),filedir,fixed=T)) stop("mountpoint 'mount' has to be a parent directory of input file 'vol.in'")
  vol.tmp=tempfile(tmpdir=filedir)
  if(file.access(filedir,mode=2)<0) stop(paste("vol2bird requires write permission in",filedir))
  if(startContainer(normalizePath(mount,winslash="/"))!=0) stop(paste("failed to start vol2bird Docker container"))

  # prepare docker input filenames relative to mountpoint
  prefixstart=if(mount=="/") 1 else 2
  prefix=substring(filedir,prefixstart+nchar(normalizePath(mount,winslash="/")))
  if(nchar(prefix)>0) prefix=paste(prefix,"/",sep="")
  vol.in.docker=paste(prefix,basename(vol.in),sep="")
  vol.tmp.docker=paste(prefix,basename(vol.tmp),sep="")

  # run vol2bird container
  if(.Platform$OS.type=="unix") result = system(paste("docker exec vol2bird bash -c 'cd data && rsl2odim ",vol.in.docker,vol.tmp.docker,"'"),ignore.stdout=!verbose)
  else result = system(paste("docker exec vol2bird bash -c \"cd data && rsl2odim ",vol.in.docker,vol.tmp.docker,"\""),ignore.stdout=!verbose,show.output.on.console = TRUE)
  if(result!=0){
    stop("failed to run rsl2odim in Docker container")
  }

  # return filename of generated temporary file
  return(vol.tmp)
}

#' Read vertical profiles from vol2bird stdout
#'
#' @param file A text file containing the standard output (stdout) generated by vol2bird
#' @param radar string containing a radar identifier
#' @param wavelength radar wavelength in cm, or one of 'C' or 'S' for C-band and S-band radar, respectively
#' @export
#' @return an object inhereting from class "\code{vpts}", see \link[bioRad]{vpts} for details
#' @examples
#' # locate example file:
#' VPtable <- system.file("extdata", "VPtable.txt", package="bioRad")
#' # load time series:
#' ts=readvp.table(VPtable,radar="KBGM", wavelength='S')
#' ts
readvp.table=function(file,radar,wavelength='C'){
  if(!file.exists(file)) stop(paste("file",file,"doesn't exist"))
  if(missing(radar)) stop("'radar' argument missing. Required to specify a radar identifier")
  if(missing(wavelength)) warning(paste("No 'wavelength' argument provided, assuming radar operates at ",wavelength,"-band",sep=""))
  if(wavelength=='C') wavelength=5.3
  if(wavelength=='S') wavelength=10.6
  if(!is.numeric(wavelength) || length(wavelength)>1) stop("not a valid 'wavelength' argument")
  #header of the data file
  header.names=c("Date","Time","HGHT","u","v","w","ff","dd","sd_vvp","gap","dbz","eta","dens","DBZH","n","n_dbz","n_all","n_dbz_all")
  #read the data
  data=read.table(file=file, header = F, col.names=header.names)
  # convert Time into a POSIXct date-time
  data$datetime <- as.POSIXct(paste(data$Date, sprintf('%04d', data$Time), sep = ""), format = "%Y%m%d%H%M", tz='UTC')
  data$Date<-NULL
  data$Time<-NULL
  # sort
  data=data[with(data, order(datetime, HGHT)),]
  # remove duplicates
  data=unique(data)
  # split into profiles
  data=split(data,data$datetime)
  names(data)<-NULL
  # verify that profiles can be flattened
  datadim=sapply(1:length(data), function(x) dim(data[[x]]))
  if(length(unique(datadim[1,]))>1){
    mostFrequent=sort(table(datadim[1,]),decreasing=T)[1]
    if(mostFrequent<=1) stop("Profiles are of unequal altitudinal dimensions, unable to merge")
    mostFrequentNBins=as.integer(names(mostFrequent))
    warning(paste("Profiles are of unequal altitudinal dimensions or contain duplicates. Discarding",length(data)-mostFrequent,"of",length(data),"profiles, restricting to",mostFrequentNBins,"altitude bins."))
    data=data[datadim[1,]==mostFrequentNBins]
  }
  # strip the datetime field
  dates=.POSIXct(sapply(1:length(data),function(x) data[[x]]$datetime[1]),tz="UTC")
  data=lapply(data, function(x) { x["datetime"] <- NULL; x })
  # check whether the time series is regular
  difftimes=difftime(dates[-1],dates[-length(dates)],units="secs")
  if(length(unique(difftimes))==1) regular = T else regular = F
  # flatten the profiles
  profile.quantities=names(data[[1]])
  vpsFlat=lapply(profile.quantities, function(quantity) sapply(data,'[[',quantity))
  names(vpsFlat)=profile.quantities
  vpsFlat$HGHT<-NULL
  # prepare output
  heights=data[[1]]$"HGHT"
  interval=unique(heights[-1]-heights[-length(heights)])

  attributes=list(where=data.frame(interval=interval,levels=length(heights)),how=data.frame(wavelength=wavelength))
  output=list(radar=radar,dates=dates,heights=heights,daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC"),timesteps=difftimes,data=vpsFlat,attributes=attributes,regular=regular)
  class(output)="vpts"
  output
}

#' Regularize a time series
#'
#' Projects objects of class \code{vpts} on a regular time grid
#' @param ts an object inhereting from class \code{vpts}, see \link[bioRad]{vpts} for details
#' @param interval time interval grid to project on. When '\code{auto}' the median interval in the time series is used
#' @param units optional units of \code{interval}, one of 'secs', 'mins', 'hours','days', 'weeks'. Defaults to 'mins'.
#' @param fill logical. Whether to fill missing timesteps with the values of the closest neighbouring profile
#' @param verbose logical. When \code{TRUE} prints text to console
#' @export
#' @return an object of class \code{vpts} with regular time steps
#' @examples
#' # locate example file:
#' VPtable <- system.file("extdata", "VPtable.txt", package="bioRad")
#' # load time series:
#' ts=readvp.table(VPtable,radar="KBGM", wavelength='S')
#' # regularize the time series on a 5 minute interval grid
#' tsRegular=regularize(ts, interval=5)
regularize=function(ts,interval="auto",units="mins",fill=F,verbose=T){
  stopifnot(inherits(ts, "vpts"))
  if (!(units %in% c("secs", "mins", "hours","days", "weeks"))) stop("invalid 'units' argument. Should be one of c('secs', 'mins', 'hours','days', 'weeks')")
  if (interval!="auto" && !is.numeric(interval)) stop("invalid or missing 'interval' argument. Should be a numeric value")
  if (length(units)>1) stop("invalid or missing 'units' argument.")
  if (!is.logical(fill) || length(fill)>1) stop("fill argument should be a logical value")
  if(interval=="auto"){
    dt=as.difftime(median(ts$timesteps),units="secs")
    if(verbose) cat(paste("projecting on",dt,"seconds interval grid...\n"))
  }
  else dt=as.difftime(interval,units=units)
  grid=seq(from=ts$daterange[1],to=ts$daterange[2],by=dt)
  index=sapply(grid,function(x) which.min(abs(ts$dates - x)))
  quantity.names=names(ts$data)
  ts$data=lapply(1:length(ts$data),function(x) ts$data[[x]][,index])
  if(!fill){
    index2=which(abs(ts$dates[index] - grid)>as.double(dt,units="secs"))
    ts$data=lapply(1:length(ts$data),function(x) {
        tmp=ts$data[[x]]
        tmp[,index2]<-NA
        tmp
      }
    )
  }
  names(ts$data)=quantity.names
  ts$dates=grid
  ts$timesteps=rep(as.double(dt,units="secs"),length(grid)-1)
  ts$regular=T
  return(ts)
}

#' Migration traffic rate
#'
#' Migration traffic rate (MTR) for an altitude layer, defined as the
#' number of targets crossing a 1 km line perpendicular to the migratory movement per hour
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @param alt.min minimum altitude in m
#' @param alt.max maximum altitude in m
#' @export
#' @examples
#' ### MTR for a single vertical profile ###
#' mtr(VP)
#'
#' ### MTRs for a list of vertical profiles ###
#' mtr(c(VP,VP))
#'
#' ### MTRs for a time series of vertical profiles ###
#' # locate example file:
#' VPtable <- system.file("extdata", "VPtable.txt", package="bioRad")
#' # load time series:
#' ts=readvp.table(VPtable,radar="KBGM", wavelength='S')
#' # print migration traffic rates:
#' mtr(ts)
#' # plot migration traffic rates for the full air column:
#' plot(mtr(ts),type='l',xlab="time [UTC]",ylab="MTR [birds/km/h]")
#' #' plot migration traffic rates for altitudes > 1 km above sea level
#' plot(mtr(ts,alt.min=1000),type='l',xlab="time [UTC]",ylab="MTR [birds/km/h]")
mtr <- function (x, alt.min, alt.max) UseMethod("mtr", x)

#' @describeIn mtr MTR of a vertical profile
#' @return class \code{vp}: the migration traffic rate (MTR) individuals/km/h
#' @export
mtr.vp = function(x,alt.min=0,alt.max=Inf){
  stopifnot(inherits(x,"vp"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))
  interval=x$attributes$where$interval
  index=which(x$data$HGHT>alt.min & x$data$HGHT<alt.max)
  mtr=sum(x$data$dens[index] * x$data$ff[index] * interval/1000,na.rm=T)
  return(mtr)
}

#' @describeIn mtr MTR of a list of vertical profiles
#' @return class \code{vplist}: a numeric atomic vector with migration traffic rates in individuals/km/h
#' @export
mtr.vplist = function(x,alt.min=0,alt.max=Inf){
  stopifnot(inherits(x,"vplist"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))
  mtr=sapply(x,mtr.vp,alt.min=alt.min,alt.max=alt.max)
  return(mtr)
}

#' @describeIn mtr MTR of a time series of vertical profiles
#' @return class \code{vpts}: a data frame with dates and migration traffic rates in individuals/km/h
#' @export
mtr.vpts = function(x,alt.min=0,alt.max=Inf){
  stopifnot(inherits(x,"vpts"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))
  interval=x$attributes$where$interval
  index=which(x$heights>alt.min & x$heights<alt.max)
  mtr=colSums(x$data$ff[index,]*x$data$dens[index,],na.rm=T)*interval/1000
  output=data.frame(dates=x$dates,mtr=mtr)
  rownames(output)=NULL
  return(output)
}

#' Class 'vp': vertical profile
#'
#' Class for vertical profiles
#' @param object object of class 'vp'
#' @param x object of class 'vp'
#' @param ... additional arguments affecting the summary produced.
#' @export
#' @rdname summary.vp
#' @method summary vp
#' @details
#' An object of class \code{vp} is a list containing
#' \describe{
#'  \item{\strong{\code{radar}}}{the radar identifier}
#'  \item{\strong{\code{datetime}}}{the nominal time of the profile [UTC]}
#'  \item{\strong{\code{data}}}{the profile data, a list containing:
#'    \describe{
#'        \item{\code{HGHT}}{height above mean sea level [m]. Alt. bin from HGHT to HGHT+interval)}
#'        \item{\code{u}}{speed component west to east [m/s]}
#'        \item{\code{v}}{speed component north to south [m/s]}
#'        \item{\code{w}}{vertical speed (unreliable!) [m/s]}
#'        \item{\code{ff}}{horizontal speed [m/s]}
#'        \item{\code{dd}}{direction [degrees, clockwise from north]}
#'        \item{\code{sd_vvp}}{VVP radial velocity standard deviation [m/s]}
#'        \item{\code{gap}}{Angular data gap detected [T/F]}
#'        \item{\code{dbz}}{Bird reflectivity factor [dBZ]}
#'        \item{\code{eta}}{Bird reflectivity [cm^2/km^3]}
#'        \item{\code{dens}}{Bird density [birds/km^3]}
#'        \item{\code{DBZH}}{Total reflectivity factor (bio+meteo scattering) [dBZ]}
#'        \item{\code{n}}{number of points VVP bird velocity analysis (u,v,w,ff,dd)}
#'        \item{\code{n_dbz}}{number of points bird density estimate (dbz,eta,dens)}
#'        \item{\code{n_all}}{number of points VVP st.dev. estimate (sd_vvp)}
#'        \item{\code{n_all_dbz}}{number of points total reflectivity estimate (DBZH)}
#'    }
#'  }
#'  \item{\strong{\code{attributes}}}{list with the profile's \code{\\what}, \code{\\where} and \code{\\how} attributes}
#' }
summary.vp=function(object, ...) print.vp(object)

#' @rdname summary.vp
#' @export
#' @return for \code{is.vp}: \code{TRUE} if its argument is of class "\code{vp}"
is.vp <- function(x) inherits(x, "vp")

#' @rdname summary.vp
#' @export
#' @return for \code{dim.vp}: dimensions of the profile data
dim.vp <- function(x) {
  stopifnot(inherits(x,"vp"))
  dim(x$data)
}

#' @rdname summary.vplist
#' @export
#' @return for \code{is.vplist}: \code{TRUE} if its argument is of class "\code{vplist}"
is.vplist <- function(x) inherits(x, "vplist")


#' Class 'vpts': time series of vertical profiles
#'
#' Class for single-site time series of vertical profiles
#' @param object object of class 'vpts'
#' @param x object of class 'vpts'
#' @param ... additional arguments affecting the summary produced.
#' @export
#' @method summary vpts
#' @details
#' details to be written
summary.vpts=function(object, ...) print.vpts(object)


#' @rdname summary.vpts
#' @export
#' @return for \code{is.vpts}: \code{TRUE} if its argument is of class "\code{vpts}"
is.vpts <- function(x) inherits(x, "vpts")

#' @rdname summary.vpts
#' @export
#' @return for \code{dim.vpts}: dimensions of the time series
dim.vpts <- function(x) {
  stopifnot(inherits(x,"vpts"))
  data.dim=dim(x$data[[1]])
  c(data.dim,length(x$data))
}

#' @rdname summary.vpts
#' @param i indices specifying elements to extract
#' @export
`[.vpts` <- function(x,i) {
  stopifnot(inherits(x,"vpts"))
  if(length(i)<2) stop("Time series should consist more than one profile")
  x$dates=x$dates[i]
  x$daterange=.POSIXct(c(min(x$dates),max(x$dates)),tz="UTC")
  x$timesteps=difftime(x$dates[-1],x$dates[-length(x$dates)],units="secs")
  if(length(unique(x$timesteps))==1) x$regular = T else x$regular = F
  quantity.names=names(x$data)
  x$data=lapply(names(x$data),function(quantity) getElement(x$data,quantity)[,i])
  names(x$data)=quantity.names
  return(x)
}

#' Radar cross section
#'
#' Gives the currently assumed radar cross section in cm^2.
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @export
#' @return a radar cross section in cm^2
#' @examples
#' # extract RCS for a single vertical profile:
#' rcs(VP)
rcs <- function (x) UseMethod("rcs", x)

#' @describeIn rcs radar cross section of a vertical profile
#' @export
rcs.vp <- function (x){
  stopifnot(inherits(x,"vp"))
  x$attributes$how$rcs_bird
}

#' @describeIn rcs radar cross sections for a list of vertical profiles
#' @export
rcs.vplist <- function (x){
  stopifnot(inherits(x,"vplist"))
  output=sapply(x,`rcs.vp`)
  output
}

#' @describeIn rcs radar cross section of a time series of vertical profile
#' @export
rcs.vpts <- function (x){
  stopifnot(inherits(x,"vpts"))
  x$attributes$how$rcs_bird
}

#' Set radar cross section
#'
#' Sets the assumed radar cross section in cm^2. This method also updates the migration densities in \code{x$data$dens}
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @param value the cross section value to assign
#' @export
#' @examples
#' # change RCS for a single vertical profile:
#' rcs(VP)<-20
`rcs<-` <- function (x, value) UseMethod("rcs<-", x)

#' @rdname rcs-set

#' @export
`rcs<-.vp` <- function(x,value){
  stopifnot(inherits(x,"vp"))
  x$attributes$how$rcs_bird=value
  x$data$dens=x$data$eta/value
  if(is.numeric(x$attributes$how$sd_vvp_thresh)){
    x$data$dens[x$data$sd_vvp<x$attributes$how$sd_vvp_thresh]=0
  }
  else{
    warning("threshold for sd_vvp not set, defaulting to 2 m/s")
    x$attributes$how$sd_vvp_thresh=2
    x$data$dens[x$data$sd_vvp<2]=0
  }
  x
}

#' @rdname rcs-set
#' @export
`rcs<-.vplist` <- function(x,value){
  stopifnot(inherits(x,"vplist"))
  output=lapply(x,`rcs<-.vp`,value=value)
  class(output)="vplist"
  output
}

#' @rdname rcs-set
#' @export
`rcs<-.vpts` <- function(x,value){
  stopifnot(inherits(x,"vpts"))
  x$attributes$how$rcs_bird=value
  x$data$dens=x$data$eta/value
  if(is.numeric(x$attributes$how$sd_vvp_thresh)){
    x$data$dens[x$data$sd_vvp<x$attributes$how$sd_vvp_thresh]=0
  }
  else{
    warning("threshold for sd_vvp not set, defaulting to 2 m/s")
    x$attributes$how$sd_vvp_thresh=2
    x$data$dens[x$data$sd_vvp<2]=0
  }
  x
}

#' threshold VVP-retrieved radial velocity standard deviation
#'
#' Gives the current threshold in VVP-retrieved radial velocity standard deviation in m/s.
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @export
#' @return threshold for \code{sd_vvp} in m/s.
#' @examples
#' # extract threshold for a single vertical profile:
#' sd_vvp(VP)
sd_vvp <- function (x) UseMethod("sd_vvp", x)

#' @describeIn sd_vvp threshold in VVP-retrieved radial velocity standard deviation of a vertical profile
#' @export
sd_vvp.vp <- function (x){
  stopifnot(inherits(x,"vp"))
  x$attributes$how$sd_vvp_thresh
}

#' @describeIn sd_vvp threshold in VVP-retrieved radial velocity standard deviation of a list of vertical profiles
#' @export
sd_vvp.vplist <- function (x){
  stopifnot(inherits(x,"vplist"))
  output=sapply(x,`sd_vvp.vp`)
  output
}

#' @describeIn sd_vvp threshold in VVP-retrieved radial velocity standard deviation of a time series of vertical profiles
#' @export
sd_vvp.vpts <- function (x){
  stopifnot(inherits(x,"vpts"))
  x$attributes$how$sd_vvp_thresh
}

#' Set threshold for VVP-retrieved radial velocity standard deviation
#'
#' Sets the threshold in \code{sd_vvp}. Altitude layers with \code{sd_vvp} below this threshold are
#' assumed to have an aerial density of zero individuals. This method updates the migration densities in \code{x$data$dens}
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @param value the value to assign
#' @export
#' @examples
#' # change threshold for a single vertical profile:
#' sd_vvp(VP)<-2
`sd_vvp<-` <- function (x, value) UseMethod("sd_vvp<-", x)

#' @rdname sd_vvp-set
#' @method sd_vvp<- vp

#' @export
`sd_vvp<-.vp` <- function(x,value){
  stopifnot(inherits(x,"vp"))
  x$attributes$how$sd_vvp_thresh=value
  if(is.numeric(x$attributes$how$rcs_bird)){
    x$data$dens=x$data$eta/x$attributes$how$rcs_bird
    x$data$dens[x$data$sd_vvp<value]=0
  }
  else{
    warning("radar cross section not set, defaulting to 11 cm^2 ...")
    x$data$dens=x$data$eta/11
    x$attributes$how$rcs_bird=11
    x$data$dens[x$data$sd_vvp<value]=0
  }
  x
}

#' @rdname sd_vvp-set
#' @export
`sd_vvp<-.vplist` <- function(x,value){
  stopifnot(inherits(x,"vplist"))
  output=lapply(x,`sd_vvp<-.vp`,value=value)
  class(output)="vplist"
  output
}

#' @rdname sd_vvp-set
#' @export
`sd_vvp<-.vpts` <- function(x,value){
  stopifnot(inherits(x,"vpts"))
  x$attributes$how$sd_vvp_thresh=value
  if(is.numeric(x$attributes$how$rcs_bird)){
    x$data$dens=x$data$eta/x$attributes$how$rcs_bird
    x$data$dens[x$data$sd_vvp<value]=0
  }
  else{
    warning("radar cross section not set, defaulting to 11 cm^2 ...")
    x$data$dens=x$data$eta/11
    x$attributes$how$rcs_bird=11
    x$data$dens[x$data$sd_vvp<value]=0
  }
  x
}


#' Migration traffic
#'
#' Total migration traffic, which is calculated by time-integration
#' of migration traffic rates. Migration traffic gives the number of individuals
#' that have passed per km perpendicular to the migratory direction at the
#' position of the radar for the full period of the time series
#' within the specified altitude band.
#' @param x an object inhereting from class '\code{vpts}'
#' @inheritParams mtr
#' @export
#' @return a numeric value equal to migration traffic in number of individuals / km
#' @examples
#' # get example time series object
#' data(VPTS)
#' VPTS
#' # total migration traffic in full altitude band
#' mt(VPTS)
#' # total migration traffic in 0-1000 meter band
#' mt(VPTS,alt.min=0,alt.max=1000)
mt <- function(x,alt.min=0, alt.max=Inf){
  stopifnot(inherits(x,"vpts"))
  dt=(c(0,x$timesteps)+c(x$timesteps,0))/2
  # convert to hours
  dt=as.numeric(dt)/3600
  sum(dt*mtr(x,alt.min,alt.max)$mtr)
}

#' Cumulative migration traffic
#'
#' Cumulative migration traffic is calculated as the cumulative sum
#' of the migration traffic within each time step of a time series.
#' Cumulative migration traffic gives the number of individuals
#' that have passed per km perpendicular to the migratory direction at the
#' position of the radar as a function oftime from the start of time series
#' within the specified altitude band.
#' @param x an object inhereting from class '\code{vpts}'
#' @inheritParams mtr
#' @export
#' @return a numeric value equal to migration traffic in number of individuals / km
#' @examples
#' # get the VPTS example dataset:
#' data(VPTS)
#' # print cumulative migration traffic to console:
#' cmt(VPTS)
#' # plot cumulative migration traffic:
#' plot(cmt(VPTS),type='l',xlab="time [UTC]",ylab="CMT [birds/km]")
cmt <- function(x,alt.min=0, alt.max=Inf){
  stopifnot(inherits(x,"vpts"))
  dt=(c(0,x$timesteps)+c(x$timesteps,0))/2
  # convert to hours
  dt=as.numeric(dt)/3600
  mtrs=mtr(x,alt.min,alt.max)
  data.frame(dates=mtrs$dates,cmt=cumsum(dt*mtrs$mtr))
}

# function obtained via Hidde Leijnse, source unknown
#' Calculate sunrise and sunset
#' @param lon longitude in decimal degrees
#' @param lat latitude in decimal degrees
#' @param date date inhereting from class "\code{POSIXt}" or a string interpretable by \link[base]{as.Date}
#' @param elev sun elevation in degrees
#' @param rise whether to output for rising or setting sun
#' @export
#' @return the moment of sunrise or sunset in UTC time
#' @details The angular diameter of the sun is about 0.536 degrees, therefore the moment
#' of sunrise/sunset corresponds to half that elevation at -0.268 degrees.
#'
#' Note that for a given date and location, sunrise time can be after sunset time when
#' the moments of sunset and sunrise are not on the same day within the UTC time zone.
#'
#' Approximate astronomical formula are used, therefore the moment of sunrise / sunset may
#' be off by a few minutes
#' @examples
#' # sunrise in the Netherlands
#' suntime(5,53,"2016-01-01")
#' # sunset in the Netherlands
#' suntime(5,53,"2016-01-01",rise=FALSE)
#' # civil twilight in Ithaca, NY, today
#' suntime(-76.5,42.4,Sys.time(),elev=-6)
suntime = function(lon, lat, date, elev=-0.268, rise = TRUE)
{
  dateOnly=as.Date(date)
  #Convert date to julian day
  yyyy = as.numeric(format(dateOnly,"%Y"))
  mm = as.numeric(format(dateOnly,"%m"))
  dd = as.numeric(format(dateOnly,"%d"))
  jy=yyyy

  if (any(jy == 0)) stop("get_time_sun: there is no year zero!")
  jy[jy < 0] = jy[jy < 0] + 1
  jm = mm
  jm[mm > 2] = mm[mm > 2] + 1
  jy[mm <= 2] = jy[mm <= 2] - 1
  jm[mm <= 2] = mm[mm <= 2] + 13
  julday = floor(365.25 * jy) + floor(30.6001 * jm) + dd + 1720995
  julday[(dd + 31 * (mm + 12 * yyyy)) >= (15 + 31 * (10 + 12 * 1582))] = julday[(dd + 31 * (mm + 12 * yyyy)) >= (15 + 31 * (10 + 12 * 1582))] + 2 - floor(0.01 * jy[(dd + 31 * (mm + 12 * yyyy)) >= (15 + 31 * (10 + 12 * 1582))]) + floor(0.25 * floor(0.01 * jy[(dd + 31 * (mm + 12 * yyyy)) >= (15 + 31 * (10 + 12 * 1582))]))
  julday0 = 2451545	#Julian day for 20000101

  #Calculation of eclips coordinates
  MeanLon = 280.460 + 0.9856474 * (julday - julday0)
  MeanAnom = 357.528 + 0.9856003 * (julday - julday0)
  EclipLon = MeanLon + 1.915 * sin(MeanAnom * pi / 180) + 0.020 * sin(2 * MeanAnom * pi / 180)
  EclipLon = EclipLon * pi / 180
  Obliquity = 23.439 - 0.0000004 * (julday - julday0)
  Obliquity = Obliquity * pi / 180

  #Calculation of the celestial coordinates of the sun
  RightAsc = atan2(cos(Obliquity) * sin(EclipLon), cos(EclipLon))
  Declinat = asin(sin(Obliquity) * sin(EclipLon))

  #Calculation of current, local hour angle
  acos_arg = (sin(elev * pi / 180) - sin(Declinat) * sin(lat * pi / 180)) / (cos(Declinat) * cos(lat * pi / 180))
  angleH = seq(1, 1, length.out = length(acos_arg)) * NA
  angleH[abs(acos_arg) <= 1] = acos(acos_arg[abs(acos_arg) <= 1])

  #Determine sign of the derivative to see if the sun is rising or setting
  if (rise) sign_angle = 1
  else sign_angle = -1
  sign_angle = -1 * sign_angle * sign(cos(Declinat) * cos(lat * pi / 180) * sin(angleH))
  sign_angle[sign_angle == 0] = 1

  #Determine time
  GMST = (sign_angle * angleH - lon * pi / 180 + RightAsc) / 15
  hour = GMST * 180 / pi - 6.697375 - 0.0657098242 * (julday - julday0)
  hour = hour - floor(hour / 24) * 24

  output=as.POSIXct(as.POSIXlt(dateOnly,tz='UTC'))+3600*hour
  return(output)
}

#' Radar beam height
#'
#' Calculates the height of a radar beam as a function of elevation and range, assuming the beam
#' is emitted at surface level.
#' @param range numeric. Range (distance from the radar antenna) in km
#' @param elev numeric. Elevation in degrees
#' @param k standard refraction coefficient
#' @param re Earth equatorial radius in km
#' @param rp Earth polar radius in km
#' @param lat geodetic latitude in degrees
#' @return numeric value. Beam height in km
#' @export
#' @details To account for refraction of the beam towards the earth's surface, an effective earth's radius of 4/3 * (true radius) is assumed.
#'
#' The earth's radius is approximated as a point on a spheroid surface, with \code{re}
#' the longer equatorial radius, and \code{rp} the shorter polar radius.
#' Typically uncertainties in refraction coefficient are relatively large, making oblateness of the
#' earth and the dependence of earth radius with latitude only a small correction.
#' Using default values assumes an average earth's radius of 6371 km.
beamheight=function(range,elev,k=4/3,lat=35,re=6378,rp=6357) sqrt(range^2+(k*earthradius(re,rp,lat))^2+2*range*(k*earthradius(re,rp,lat))*sin(elev*pi/180))-k*earthradius(re,rp,lat)

earthradius=function(a,b,latdeg){
  lat=latdeg*pi/180
  sqrt(((a^2*cos(lat))^2+(b^2*sin(lat))^2)/((a*cos(lat))^2+(b*sin(lat))^2))
}

#' Radar beam width
#'
#' Calculates the width of a radar beam as a function of range and beam angle
#' @param range numeric. Range (distance from the radar antenna) in km
#' @param angle numeric. Beam angle in degrees
#' @return numeric value. Beam width in m
#' @export
beamwidth=function(range,angle=1) range*1000*sin(angle*pi/180)



