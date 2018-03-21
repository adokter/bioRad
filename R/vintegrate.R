#' Vertically integrate profiles
#'
#' Performs a vertical integration of density, reflectivity and migration traffic rate,
#' and a vertical averaging of ground speed and direction weighted by density.
#' @param x a \code{vp}, \code{vplist} or \code{vpts} object
#' @param alt.min minimum altitude in m
#' @param alt.max maximum altitude in m
#' @param alpha migratory direction in clockwise degrees from north
#' @export
#' @return an object of class \code{vivp}, a data frame with vertically integrated profile quantities
#' @details
#' The function generates a specially classed data frame with the following quantities
#' \describe{
#'    \item{\code{datetime}}{POSIXct date of each profile in UTC}
#'    \item{\code{vid}}{Vertically Integrated Density in individuals/km^2. \code{vid} is a surface density,
#'          whereas \code{dens} in \code{vp} objects is a volume density.}
#'    \item{\code{vir}}{Vertically Integrated Reflectivity in cm^2/km^2}
#'    \item{\code{mtr}}{Migration Traffic Rate in individuals/km/h}
#'    \item{\code{rtr}}{Reflectivity Traffic Rate in cm^2/km/h}
#'    \item{\code{ff}}{Horizontal ground speed in m/s}
#'    \item{\code{dd}}{Horizontal ground speed direction in degrees}
#'    \item{\code{u}}{Ground speed component west to east in m/s}
#'    \item{\code{v}}{Ground speed component north to south in m/s}
#'    \item{\code{HGHT}}{Height above sea level in m}
#' }
#' Vertically integrated density and reflectivity are related according to \eqn{vid=vir/rcs(x)}, with \link{rcs}
#' the assumed radar cross section per individual. Similarly, migration traffic rate and reflectivity
#' traffic rate are related according to \eqn{mtr=rtr/rcs(x)}
#'
#' See \link{mtr} for further information on the definition of migration traffic rate.
#' @examples
#' ### MTR for a single vertical profile ###
#' vintegrate(VP)
#'
#' ### MTRs for a list of vertical profiles ###
#' vintegrate(c(VP,VP))
#'
#' ### MTRs for a time series of vertical profiles ###
#' # load example data:
#' data(VPTS)
#' VPTS
#' # print migration traffic rates:
#' vivp=vintegrate(VPTS)
#' # plot migration traffic rates for the full air column:
#' plot(VPTS)
#' #' plot migration traffic rates for altitudes > 1 km above sea level
#' plot(vintegrate(VPTS,alt.min=1000))
vintegrate <- function(x, alt.min, alt.max, alpha=NA) UseMethod("vintegrate", x)

#' @describeIn vintegrate Vertically integrate a vertical profile
#' @export
vintegrate.vp = function(x,alt.min=0,alt.max=Inf, alpha=NA){
  stopifnot(inherits(x,"vp"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))
  stopifnot(is.na(alpha) || is.numeric(alpha))
  interval=x$attributes$where$interval
  index=which(x$data$HGHT>=alt.min & x$data$HGHT<alt.max)
  if(is.na(alpha)) cosfactor=rep(1,length(index))
  else cosfactor = cos((fetch(x,"dd")[index]-alpha)*pi/180)
  # multiply speeds by 3.6 to convert m/s to km/h
  mtr=sum(fetch(x,"dens")[index] * cosfactor * fetch(x,"ff")[index] * 3.6 * interval/1000,na.rm=T)
  rtr=sum(fetch(x,"eta")[index] * cosfactor * fetch(x,"ff")[index] * 3.6 * interval/1000,na.rm=T)
  vid=sum(fetch(x,"dens")[index],na.rm=T)*interval/1000
  vir=sum(fetch(x,"eta")[index],na.rm=T)*interval/1000
  height=sum((x$heights[index]+x$attributes$where$interval/2)*fetch(x,"dens")[index],na.rm=T)/sum(fetch(x,"dens")[index],na.rm=T)
  u=sum(fetch(x,"u")[index]*fetch(x,"dens")[index],na.rm=T)/sum(fetch(x,"dens")[index],na.rm=T)
  v=sum(fetch(x,"v")[index]*fetch(x,"dens")[index],na.rm=T)/sum(fetch(x,"dens")[index],na.rm=T)
  ff=sqrt(u^2+v^2)
  dd=(pi/2-atan2(v,u))*180/pi
  output=data.frame(datetime=x$datetime,mtr=mtr,vid=vid,vir=vir,rtr=rtr,ff=ff,dd=dd,u=u,v=v,HGHT=height)
  class(output)=c("vivp","data.frame")
  rownames(output)=NULL
  attributes(output)$alt.min=alt.min
  attributes(output)$alt.max=alt.max
  attributes(output)$alpha=alpha
  attributes(output)$rcs=rcs(x)
  attributes(output)$lat=x$attributes$where$lat
  attributes(output)$lon=x$attributes$where$lon
  return(output)
}

#' @describeIn vintegrate Vertically integrate a list of vertical profiles
#' @export
vintegrate.vplist = function(x,alt.min=0,alt.max=Inf,alpha=NA){
  stopifnot(inherits(x,"vplist"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))
  output=do.call(rbind,lapply(x,vintegrate.vp,alt.min=alt.min,alt.max=alt.max,alpha=alpha))
  class(output)=c("vivp","data.frame")
  attributes(output)$alt.min=alt.min
  attributes(output)$alt.max=alt.max
  attributes(output)$alpha=alpha
  attributes(output)$rcs=rcs(x)
  #TODO set lat/lon attributes
  return(output)
}

#' @describeIn vintegrate Vertically integrate a time series of vertical profiles
#' @export
vintegrate.vpts <- function(x,alt.min=0,alt.max=Inf,alpha=NA){
  stopifnot(inherits(x, "vpts"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))
  stopifnot(is.na(alpha) || is.numeric(alpha))
  interval=x$attributes$where$interval
  index=which(x$heights>=alt.min & x$heights<alt.max)
  if(is.na(alpha)) cosfactor=1+0*fetch(x,"dd")[index,]
  else cosfactor = cos((fetch(x,"dd")[index,]-alpha)*pi/180)
  # multiply speeds by 3.6 to convert m/s to km/h
  mtr=colSums(cosfactor*fetch(x,"ff")[index,]*3.6*fetch(x,"dens")[index,],na.rm=T)*interval/1000
  rtr=colSums(cosfactor*fetch(x,"ff")[index,]*3.6*fetch(x,"eta")[index,],na.rm=T)*interval/1000
  vid=colSums(fetch(x,"dens")[index,],na.rm=T)*interval/1000
  vir=colSums(fetch(x,"eta")[index,],na.rm=T)*interval/1000
  height=colSums((x$heights[index]+x$attributes$where$interval/2)*fetch(x,"dens")[index,],na.rm=T)/colSums(fetch(x,"dens")[index,],na.rm=T)
  u=colSums(fetch(x,"u")[index,]*fetch(x,"dens")[index,],na.rm=T)/colSums(fetch(x,"dens")[index,],na.rm=T)
  v=colSums(fetch(x,"v")[index,]*fetch(x,"dens")[index,],na.rm=T)/colSums(fetch(x,"dens")[index,],na.rm=T)
  ff=sqrt(u^2+v^2)
  dd=(pi/2-atan2(v,u))*180/pi
  output=data.frame(datetime=x$dates,mtr=mtr,vid=vid,vir=vir,rtr=rtr,ff=ff,dd=dd,u=u,v=v,HGHT=height)
  class(output)=c("vivp","data.frame")
  rownames(output)=NULL
  attributes(output)$alt.min=alt.min
  attributes(output)$alt.max=alt.max
  attributes(output)$alpha=alpha
  attributes(output)$rcs=rcs(x)
  attributes(output)$lat=x$attributes$where$lat
  attributes(output)$lon=x$attributes$where$lon
  return(output)
}




