#' Plot a vertical profile
#'
#' @param x a vp class object
#' @param quantity character string with the quantity to plot.
#' See \link[=summary.vp]{vp} for list of available quantities.
#' Aerial density related: '\code{dens}','\code{eta}','\code{dbz}','\code{DBZH}' for density, reflectivity, reflectivity factor and total reflectivity factor, respectively.
#' Ground speed related: '\code{ff}','\code{dd}', for ground speed and direction, respectively.
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param line.col Color of the plotted curve
#' @param line.lwd Line width of the plotted curve
#' @param ... Additional arguments to be passed to the low level \link[graphics]{plot} plotting function
#' @export
#' @method plot vp
#' @examples
#' data(VP)
#' plot(VP)
#' plot(VP,line.col='blue')
plot.vp=function(x, quantity="dens", xlab=expression("volume density [#/km"^3*"]"),ylab="height [km]",line.col='red',line.lwd=1,...){
  stopifnot(inherits(x,"vp"))
  if(!(quantity %in% names(x$data))) stop(paste("unknown quantity '",quantity,"'",sep=""))
  # set up the plot labels
  if(missing(xlab)){
    if(quantity=="u") xlab="W->E ground speed component U [m/s]"
    if(quantity=="v") xlab="N->S ground speed component V [m/s]"
    if(quantity=="w") xlab="vertical speed W [m/s]"
    if(quantity=="ff") xlab="ground speed [m/s]"
    if(quantity=="dd") xlab="ground speed direction [deg]"
    if(quantity=="sd_vvp") xlab="VVP-retrieved radial velocity standard deviation [m/s]"
    if(quantity=="head_bl") xlab="heading baseline [unitless]"
    if(quantity=="head_ff") xlab="heading amplitude [unitless]"
    if(quantity=="head_dd") xlab="heading direction [deg]"
    if(quantity=="head_sd") xlab="heading standard deviation [unitless]"
    if(quantity=="dbz") xlab=expression("reflectivity factor [dBZ"[e] * "]")
    if(quantity=="dens") xlab=expression("volume density [#/km"^3*"]")
    if(quantity=="eta") xlab=expression("reflectivity "*eta*" [cm"^2*"/km"^3*"]")
    if(quantity=="DBZH") xlab=expression("total reflectivity factor [dBZ"[e] * "]")
    if(quantity=="gap") xlab="Angular data gap detected [logical]"
    if(quantity=="n") xlab="# range gates in VVP velocity analysis"
    if(quantity=="n_all") xlab="# range gates in sd_vvp estimate"
    if(quantity=="n_dbz") xlab="# range gates in density estimates"
    if(quantity=="n_dbz_all") xlab="# range gates in DBZH estimate"
  }

  # extract the data from the time series object
  pdat=fetch(x,quantity)
  plot(pdat,x$data$HGHT/1000,xlab=xlab,ylab=ylab,...)
  points(pdat,x$data$HGHT/1000, col=line.col,lwd=line.lwd,type="l")
}
