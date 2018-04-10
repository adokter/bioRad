#' Bind vertical profiles into time series objects
#'
#' Binds profiles and list of profiles into a time series object. Also combines multiple time series objects of a single radar into one
#' time series object.
#' @param ... object of class \code{vp} or \code{vpts}
#' @param x objects of class \code{vp}, \code{vplist} or \code{vpts}
#' @export
#' @return An object or a list of objects of class \code{vpts}, see \link{summary.vpts} for details
#' The profiles in the input \code{vpts} objects will be sorted in time in the output object.
bind <- function (x, ...) UseMethod("bind", x)

#' @describeIn bind bind \code{vp} objects into a \code{vpts} object. If \code{vp} of multiple radars are provided, a list
#' is returned containing \code{vpts} time series objects for each radar.
#' @export
bind.vp = function(...){
  vps=list(...)
  vptest=sapply(vps,function(x) is(x,"vp"))
  if(FALSE %in% vptest) stop("requires vp objects as input")
  # extract radar identifiers
  radars=unique(sapply(vps,'[[',"radar"))
  vpts(c.vp(...))
}

#' @describeIn bind bind \code{vplist} objects into a \code{vpts} object. If data of multiple radars is provided, a list
#' is returned containing \code{vpts} time series objects for each radar.
#' @export
bind.vplist = function(x, ...){
  vptest=sapply(x,function(y) is(y,"vp"))
  if(FALSE %in% vptest) stop("requires vplist object as input")
  vpts(x, ...)
}

#' @describeIn bind bind multiple time series of vertical profiles (\code{vpts} objects) into a single \code{vpts} object.
#' Requires the \code{vpts} objects to be from the same radar.
#' @param attributes.from which vpts object to copy attributes form (default: first)
#' @export
#' @examples
#' # load the example vpts object
#' data(VPTS)
#' # split the vpts object into two separate time series,
#' # one containing profile 1-10, and a second combining profile 11-20:
#' vpts1=VPTS[1:10]
#' vpts2=VPTS[11:20]
#' # use bind to merge the two together:
#' vpts1and2=bind(vpts1,vpts2)
#' # verify that the binded objected now has 20 profiles, 10 from vpts1 and 10 from vpts2:
#' summary(vpts1and2)
#' # extract two profiles
#' vp1=VPTS[1]
#' vp1
#' vp2=VPTS[2]
#' vp2
#' # bind the profile objects back into a time series object:
#' bind(vp1,vp2)
bind.vpts = function(...,attributes.from=1){
  vptss=list(...)
  vptstest=sapply(vptss,function(x) is(x,"vpts"))
  if(FALSE %in% vptstest) stop("requires vpts objects as input")
  # extract radar identifiers
  radars=unique(sapply(vptss,'[[',"radar"))
  if(length(radars)>1) stop("Vertical profiles are not from a single radar")
  if(length(unique(lapply(vptss,'[[',"heights")))>1) stop("Vertical profiles have non-aligning altitude layers")
  if(length(unique(lapply(vptss,function(x) names(x$"data"))))>1) stop("Vertical profiles have different quantities")
  # extract date-times
  dates=.POSIXct(do.call("c",lapply(vptss,'[[',"dates")),tz="UTC")
  quantities=names(vptss[[1]]$data)
  ordering=order(dates)
  dates=dates[ordering]
  data=lapply(quantities, function(quantity) do.call(cbind,lapply(vptss,function(x) x$"data"[[quantity]]))[,ordering])
  names(data)=quantities
  difftimes=difftime(dates[-1],dates[-length(dates)],units="secs")
  if(length(unique(difftimes))==1) regular = T else regular = F
  output=list(radar=radars,dates=dates,heights=vptss[[1]]$heights,daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC"),timesteps=difftimes,data=data,attributes=vptss[[attributes.from]]$attributes,regular=regular)
  class(output)="vpts"
  output
}
