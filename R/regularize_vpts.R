#' Regularize a time series of vertical profiles (\code{vpts}) on a regular time grid
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
#' ts=read_vpts(VPtable,radar="KBGM", wavelength='S')
#' # regularize the time series on a 5 minute interval grid
#' tsRegular=regularize_vpts(ts, interval=5)
regularize_vpts=function(ts,interval="auto",t.min=ts$daterange[1],t.max=ts$daterange[2],units="mins",fill=F,verbose=T){
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
