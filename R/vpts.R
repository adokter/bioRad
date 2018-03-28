#' Class 'vpts': time series of vertical profiles
#'
#' Class for single-site time series of vertical profiles
#' @param object object of class 'vpts'
#' @param x object of class 'vpts'
#' @param ... additional arguments affecting the summary produced.
#' @export
#' @method summary vpts
#' @details An object of class \code{vpts} contains time-ordered profiles of a single radar
#' station.
#'
#' The time series can be regular or irregular, indicated by the \code{regular} field
#'
#' In a regular \code{vpts} object the profiles are equally spaced in time.
#' In an irregular \code{vpts} object the time steps between profiles are of unequal length.
#'
#' Irregular time series can be projected onto a regular time grid using the \link{regularize} function.
#'
#' By contrast, in \link[=summary.vp]{vplist} objects the profiles have no time ordering, and can contain profiles of multiple radars.
#'
#' Data contained in this class object should be accessed with the \link{fetch} function.
#' Information stored under \code{attributes} (see below) can be accessed directly.
#'
#' An object of class \code{vpts} is a list containing
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

#' Subset `vpts`
#'
#' Extract by index from a vpts
#'
#' @param x object of class 'vpts'
#' @param i indices specifying elements to extract
#' @export
`[.vpts` <- function(x,i) {
  stopifnot(inherits(x,"vpts"))
  if(length(i)<1) stop("Time series should contain more than one profile")
  if(length(i)==1){
    if(i>0) return(vpts2vp(x,i))
    else{
      if(dim(x)[2]==2){
        if(i==-1) return(vpts2vp(x,2))
        if(i==-2) return(vpts2vp(x,1))
      }
    }
  }
  x$dates=x$dates[i]
  x$daterange=.POSIXct(c(min(x$dates),max(x$dates)),tz="UTC")
  x$timesteps=difftime(x$dates[-1],x$dates[-length(x$dates)],units="secs")
  if(length(unique(x$timesteps))==1) x$regular = T else x$regular = F
  quantity.names=names(x$data)
  x$data=lapply(names(x$data),function(quantity) getElement(x$data,quantity)[,i])
  names(x$data)=quantity.names
  return(x)
}

vpts2vp <- function(x,i) {
  stopifnot(inherits(x,"vpts"))
  nvp=dim(x)[2]
  if(i<1 || i>nvp) return(NA)
  vpout=list()
  vpout$radar=x$radar
  vpout$datetime=x$dates[i]
  vpout$data=as.data.frame(lapply(names(x$data),function(y) x$data[y][[1]][,i]))
  names(vpout$data)=names(x$data)
  vpout$attributes=x$attributes
  vpout$data$HGHT=x$heights
  class(vpout)="vp"
  vpout
}

#' Convert list of vertical profiles to time series (\code{vpts}) objects
#'
#' @param x An object of class \code{vplist}, usually a result of a call to \link{readvp.list}
#' @param radar optional string containing the radar identifier to generate time series for.
#' @export
#' @return an object of class \link[=summary.vpts]{vpts} when \code{vplist} contains profiles of a single radar. A list of objects of class \link[=summary.vpts]{vpts} in
#' case when \code{vplist} contains profiles of multiple radars, containing \link[=summary.vpts]{vpts} objects for each radar.
#' @rdname vpts
#' @examples
#' \dontrun{
#' vps=readvp(c("my/path/profile1.h5","my/path/profile2.h5", ...))
#' ts=vpts(vps)
#' }
vpts = function(x,radar=NA){
  stopifnot(inherits(x, "vplist"))
  # extract radar identifiers
  radars=sapply(x,'[[',"radar")
  uniqueRadars=sort(unique(radars))
  if(!is.na(radar)){
    if(!(radar %in% uniqueRadars)) stop(paste("no profiles found for radar",radar))
    else return(vptsHelper(x[which(radars==radar)]))
  }
  # extract date-times
  if(is.na(radar) & (length(uniqueRadars)==1)) return(vptsHelper(x[which(radars==uniqueRadars)]))
  else return(lapply(uniqueRadars,function(y) vpts(x[radars==y])))
}

#' print method for class \code{vpts}
#'
#' @param x An object of class \code{vpts}, usually a result of a call to \link{vpts}
#' @keywords internal
#' @export
print.vpts=function(x,digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "vpts"))
  cat("                  ",if(x$regular) "Regular" else "Irregular","time series of vertical profiles (class vpts)\n\n")
  cat("           radar: ",x$radar,"\n")
  cat("      # profiles: ",length(x$dates),"\n")
  cat("time range (UTC): ",as.character(x$daterange[1]),"-",as.character(x$daterange[2]),"\n")
  if(length(x$timesteps)>0){
    stepMin=min(x$timesteps)
    stepMax=max(x$timesteps)
  } else stepMin=stepMax=NA
  if(x$regular) cat("   time step (s): ",stepMin,"\n")
  else cat("   time step (s): ","min:",stepMin,"    max: ",stepMax,"\n")
}

vptsHelper = function(vps){
  dates=.POSIXct(do.call("c",lapply(vps,'[[',"datetime")),tz="UTC")
  daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC")
  # sort by datetime
  vps=vps[order(sapply(vps,'[[',"datetime"))]
  dates=.POSIXct(do.call("c",lapply(vps,'[[',"datetime")),tz="UTC")
  difftimes=difftime(dates[-1],dates[-length(dates)],units="secs")
  profile.quantities=names(vps[[1]]$data)

  if(length(unique(lapply(vps,'[[',"heights")))>1) stop(paste("Vertical profiles of radar",vps[[1]]$radar,"have non-aligning altitude layers"))
  if(length(unique(lapply(vps,function(x) names(x$"data"))))>1) stop(paste("Vertical profiles of radar",vps[[1]]$radar,"contain different quantities"))

  vpsFlat=lapply(profile.quantities, function(quantity) sapply(lapply(vps,'[[',"data"),'[[',quantity))
  names(vpsFlat)=profile.quantities
  if(length(unique(difftimes))==1) regular = T else regular = F
  vpsFlat$HGHT<-NULL
  output=list(radar=vps[[1]]$radar,dates=dates,heights=vps[[1]]$data$HGHT,daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC"),timesteps=difftimes,data=vpsFlat,attributes=vps[[1]]$attributes,regular=regular)
  class(output)="vpts"
  output
}

#' Regularize a time series
#'
#' Projects objects of class \code{vpts} on a regular time grid
#' @param ts an object inhereting from class \code{vpts}, see \link{vpts} for details.
#' @param interval time interval grid to project on. When '\code{auto}' the median interval in the time series is used.
#' @param t.min start time of the projected time series, as a POSIXct object. Taken from \code{ts} when '\code{auto}'.
#' @param t.max end time of the projected time series, as a POSIXct object. Taken from \code{ts} when '\code{auto}'.
#' @param units optional units of \code{interval}, one of 'secs', 'mins', 'hours','days', 'weeks'. Defaults to 'mins'.
#' @param fill logical. Whether to fill missing timesteps with the values of the closest neighbouring profile.
#' @param verbose logical. When \code{TRUE} prints text to console.
#' @export
#' @return an object of class \code{vpts} with regular time steps
#' @details Irregular time series of profiles are typically aligned on a regular time grid with the expected time interval
#' at which a radar provides data. Empty profiles with only missing data values will be inserted at time stamps of the
#' regular time grid that have no matching profile in the irregular time series. This also has the benefit that missing profiles
#' become visible in profile plots of regular time series using \link{plot.vpts}.
#' @examples
#' # locate example file:
#' VPtable <- system.file("extdata", "VPtable.txt", package="bioRad")
#' # load time series:
#' ts=readvp.table(VPtable,radar="KBGM", wavelength='S')
#' # regularize the time series on a 5 minute interval grid
#' tsRegular=regularize(ts, interval=5)
regularize=function(ts,interval="auto",t.min=ts$daterange[1],t.max=ts$daterange[2],units="mins",fill=F,verbose=T){
  stopifnot(inherits(ts, "vpts"))
  stopifnot(inherits(t.min, "POSIXct"))
  stopifnot(inherits(t.max, "POSIXct"))
  if (!(units %in% c("secs", "mins", "hours","days", "weeks"))) stop("invalid 'units' argument. Should be one of c('secs', 'mins', 'hours','days', 'weeks')")
  if (interval!="auto" && !is.numeric(interval)) stop("invalid or missing 'interval' argument. Should be a numeric value")
  if (length(units)>1) stop("invalid or missing 'units' argument.")
  if (!is.logical(fill) || length(fill)>1) stop("fill argument should be a logical value")

  if(interval=="auto"){
    dt=as.difftime(median(ts$timesteps),units="secs")
    if(verbose) cat(paste("projecting on",dt,"seconds interval grid...\n"))
  }
  else dt=as.difftime(interval,units=units)
  daterange=c(t.min,t.max)
  grid=seq(from=daterange[1],to=daterange[2],by=dt)
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

#' Coerce Vertical Profile Time Series to a Data Frame
#'
#' Converts vertical profile time series (objects of class \code{vpts}) to a Data Frame,
#' and optionally adds information on sunrise/sunset, day/night and derived quantities
#' like migration traffic rates.
#' @param x object of class vpts
#' @param row.names \code{NULL} or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional If \code{FALSE} then the names of the variables in the data frame are checked to ensure that they are syntactically valid variable names and are not duplicated.
#' @param quantities an optional character vector with the names of the quantities to include as columns in the data frame
#' @param elev sun elevation in degrees, see \link{suntime}.
#' @param lat radar latitude in decimal degrees. When set, overrides the latitude stored in \code{x} in \link{suntime} calculations
#' @param lon radar longitude in decimal degrees. When set, overrides the longitude stored in \code{x} in \link{suntime} calculations
#' @param suntime logical. When TRUE, adds sunrise/sunset and day/night information to each row
#' @param geo logical. When TRUE, adds latitude, longitude and antenna height of the radar to each row
#' @param ... additional arguments to be passed to or from methods.
#' @return an object of class data.frame
#' @export
#' @details
#' Note that only the 'dens' quantity is thresholded by the radial velocity standard deviation \link{sd_vvp}.
#' Note that this is different from the default \link{plot.vp}, \link{plot.vpts} and \link{fetch.vp} functions, where
#' quantities "eta","dbz","ff","u","v","w","dd" are all thresholded by \link{sd_vvp}.
#' @examples
#' # load an example vertical profile time series object
#' data(VPTS)
#' # convert the object to a data.frame
#' df=as.data.frame(VPTS)
#' # do not compute sunrise/sunset information
#' df=as.data.frame(VPTS,suntime=FALSE)
#' # override the latitude/longitude information stored in the object
#' # when calculating sunrise / sunset
#' df=as.data.frame(VPTS,suntime=TRUE,lat=50,lon=4)
as.data.frame.vpts = function(x, row.names = NULL, optional = FALSE, quantities=names(x$data),suntime=TRUE,geo=TRUE, elev = -0.268, lat=NULL, lon=NULL, ...){
  stopifnot(inherits(x,"vpts"))
  if(!is.null(row.names)){
    if(is.character(row.names) & length(row.names)==length(x$dates)*length(x$heights)) rownames(output)=row.names
    else stop(paste("'row.names' is not a character vector of length",length(x$dates)*length(x$heights)))
  }
  if(is.null(lat)) lat = x$attributes$where$lat
  if(is.null(lon)) lon = x$attributes$where$lon
  missing=which(!(quantities %in% names(x$data)))
  if(length(missing)>0) stop(paste(paste(quantities[missing],collapse=" "),"not an available quantity, select one or more of",paste(names(x$data),collapse=",")))
  # coerce data to a data frame
  output=as.data.frame(lapply(x$data[quantities],c),optional=optional,...)
  # add height and datetime as a column
  output=cbind(datetime=as.POSIXct(c(t(replicate(length(x$heights),x$dates))),origin="1970-1-1",tz='UTC'),height=rep(x$heights,length(x$dates)), output)
  # add radar name
  output=cbind(radar=x$radar,output,stringsAsFactors=FALSE)
  # add location information
  if(geo){
    output$lat=lat
    output$lon=lon
    output$height_antenna=x$attributes$where$height
  }
  # override the lat,lon attributes in case of user-provided values
  x$attributes$where$lat=lat
  x$attributes$where$lon=lon
  # add day
  if(suntime){
    dayQ=day(x,elev=elev)
    dayQ=c(t(replicate(length(x$heights),dayQ)))
    output=cbind(output,day=dayQ)
    sunrise=suntime(x$dates,lat=lat,lon=lon,rise=T)
    sunset=suntime(x$dates,lat=lat,lon=lon,rise=F)
    output$sunrise=as.POSIXct(c(t(replicate(length(x$heights),sunrise))),origin="1970-1-1",tz='UTC')
    output$sunset=as.POSIXct(c(t(replicate(length(x$heights),sunset))),origin="1970-1-1",tz='UTC')
  }
  output
}
