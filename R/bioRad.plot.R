r.points = c(1, 63, 82, 94, 146, 177, 192, 209, 256)
r.values = c(255, 255, 163, 255, 255, 81, 81, 0, 0)
g.points = c(1, 65, 80, 111, 143, 256)
g.values = c(255, 255, 163, 163, 0, 0)
b.points = c(1, 80, 97, 111, 128, 160, 207, 256)
b.values = c(255, 0, 0, 82, 0, 0, 255, 0)
plot.colors = rgb(c(200, approx(r.points, r.values, seq(1, 256, length.out = 255))$y), c(200, approx(g.points, g.values, seq(1, 256, length.out = 255))$y), c(200, approx(b.points, b.values, seq(1, 256, length.out = 255))$y), maxColorValue = 255)

# true when NA but not when NaN
is.na2=function(x) is.na(x) & !is.nan(x)

#plot.dBZbird = 1 + round(254 * (t(dBZbird[, 1 : (length(times) - 1)] / 10 + log10(dBZ.factor)) - contour.range[1]) / (contour.range[2] - contour.range[1]))
#plot.dBZbird[plot.dBZbird < 1] = 0

#' Plot a time series of vertical profiles
#'
#' Plot a time series of vertical profiles  of class \code{VPTimeSeries}.
#' @param x a VP class object inheriting from class \code{VPTimeSeries}
#' @param xlab a title for the x-axis
#' @param ylab a title for the y-axis
#' @param quantity character string with the quantity to plot, one of '\code{dens}','\code{eta}','\code{dbz}','\code{DBZH}' for density, reflectivity, reflectivity factor and total reflectivity factor, respectively.
#' @param log logical. Whether to display \code{quantity} data on a logarithmic scale
#' @param zlim numerical atomic vector of length 2, specifying the range of \code{quantity} values to plot
#' @param legend.ticks numeric atomic vector specifying the ticks on the color bar
#' @param main a title for the plot
#' @param ... Additional arguments to be passed to the low level \link[graphics]{image} plotting function
#' @export
#' @details
#' Profile can be visualised in three related quantities, as specified by argument \code{quantity}:
#' \describe{
#'  \item{"\code{dens}"}{the aerial density of individuals. This quantity is dependent on the assumed radar cross section (RCS) in the \code{x$attributes$how$rcs_bird} attribute}
#'  \item{"\code{eta}"}{reflectivity. This quantity is independent of the value of the \code{rcs_bird} attribute}
#'  \item{"\code{dbz}"}{reflectivity factor. This quantity is independent of the value
#'  of the \code{rcs_bird} attribute, and corresponds to the dBZ scale commonly used in weather radar meteorology.
#'  Bioscatter by birds tends to occur at much higher reflectivity factors at S-band than at C-band}
#'  \item{"\code{DBZH}"}{total reflectivity factor. This quantity equals the reflectivity factor of all scatterers (biological and meteorological scattering cambined)}
#' }
#'
#' @examples
#' # locate example file:
#' VPtable <- system.file("extdata", "VPtable.txt", package="bioRad")
#' # load and regularize time series of vertical profiles:
#' ts=regularize(readVP.table(VPtable,radar="KBGM", wavelength='S'))
#' # plot density of individuals for the first 500 time steps, in the altitude layer 0-3000 m.
#' plot(ts[1:500], ylim=c(0,3000))
#' # plot total reflectivity factor (rain,birds,insects together):
#' plot(ts[1:500], ylim=c(0,3000), quantity="DBZH")
plot.VPTimeSeries = function(x, xlab="time [UTC]",ylab="height [m]",quantity="dens",log=T, zlim, legend.ticks, main,...){
  stopifnot(inherits(x,"VPTimeSeries"))
  if(!x$regular) warning("Irregular time-series: x-axis is not a linear time scale. Use 'regularize' to make time series regular.")

  # prepare zlim, ticks and legendticks
  if(missing(zlim)) {
    if(quantity=="dens" & log){
      ticks=legendticks=c(1,2,5,10,25,50,100,200,500,1000)
      zlim=c(.5,1000)
    }
    if(quantity=="dens" & !log){
      ticks=legendticks=seq(0,500,20)
      zlim=c(0,500)
    }
    if(quantity=="eta" & log){
      ticks=legendticks=10*c(1,2,5,10,25,50,100,200,500,1000)
      zlim=c(5,10000)
    }
    if(quantity=="eta" & !log){
      ticks=legendticks=seq(0,5000,500)
      zlim=c(0,5000)
    }
    if(quantity=="dbz"){
      if(x$attributes$how$wavelength>10){
        ticks=legendticks=seq(-5,30,5)
        zlim=c(-5,30)
      }
      else{
        ticks=legendticks=seq(-20,10,5)
        zlim=c(-20,5)
      }
    }
    if(quantity=="DBZH"){
      ticks=legendticks=seq(-10,50,10)
      zlim=c(-10,30)
    }
  }
  else{
    ticks=legendticks=seq(zlim[1],zlim[2],length.out=10)
  }
  if(!missing(legend.ticks)) ticks=legendticks=legend.ticks

  # set up the plot labels
  if(missing(main)){
    if(quantity=="dens") main=expression("volume density [#/km"^3*"]")
    if(quantity=="eta") main=expression("reflectivity "*eta*" [cm"^2*"/km"^3*"]")
    if(quantity=="dbz") main=expression("reflectivity factor [dBZ"[e] * "]")
    if(quantity=="DBZH") main=expression("total reflectivity factor [dBZ"[e] * "]")
  }

  # extract the data from the time series object
  if(quantity=="dens") plotdata=t(x$data$dens)
  if(quantity=="eta") plotdata=t(x$data$eta)
  if(quantity=="dbz"){
    if(log){
      if(!missing(log)) warning("reflectivity factor 'dbz' is already logarithmic, ignoring 'log' argument...")
      log=F
    }
    plotdata=t(x$data$dbz)
  }
  if(quantity=="DBZH"){
    if(log){
      if(!missing(log)) warning("total reflectivity factor 'DBZH' is already logarithmic, ignoring 'log' argument...")
      log=F
    }
    plotdata=t(x$data$DBZH)
  }

  # do log-transformations:
  if(log){
    plotdata=log(plotdata)
    legendticks=log(ticks)
    zlim=log(zlim)
  }
  breaks=c(zlim[1]-(zlim[2]-zlim[1])/1000,seq(zlim[1],zlim[2],length.out=256))
  # move points out of zlim range into valid color range
  plotdata[plotdata<(breaks[2]+breaks[3])/2]=(breaks[2]+breaks[3])/2
  plotdata[plotdata>zlim[2]]=breaks[length(breaks)]
  plotdata[is.na2(plotdata)]=(breaks[1]+breaks[2])/2
  #
  zlim[1]=breaks[1]
  axis.args=list(at=legendticks,labels=ticks)
  # FIXME: want to change this to
  # plotdata[is.nan(plotdata)]=(breaks[2]+breaks[3])/2
  # when vol2bird stdout also differentiates between NA and NaN:
  plotdata[is.na(plotdata)]=(breaks[2]+breaks[3])/2
  # FIXME: want to change this to
  # plotdata[is.na2(plotdata)]=(breaks[1]+breaks[2])/2
  # when vol2bird stdout also differentiates between NA and NaN:
  plotdata[is.na(plotdata)]=(breaks[2]+breaks[3])/2
  image.plot(x$dates,x$heights,plotdata,col=plot.colors,xlab=xlab,ylab=ylab,axis.args=axis.args,breaks=breaks,zlim=zlim,main=main,...)
}
