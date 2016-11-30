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
#' @param barbs logical. Whether to plot overlay speed barbs
#' @param barbs.h integer. Number of barbs to plot in altitudinal dimension
#' @param barbs.t integer. Number of barbs to plot in temporal dimension
#' @param barbs.dens numeric. Lower threshold in aerial density of individuals for plotting speed barbs
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
#' In the speed barbs, each half flag represents 2.5 m/s, each full flag 5 m/s, each pennant (triangle) 25 m/s
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
plot.VPTimeSeries = function(x, xlab="time [UTC]",ylab="height [m]",quantity="dens",log=T, barbs=T, barbs.h=10, barbs.t=20, barbs.dens=5, zlim, legend.ticks, main, ...){
  stopifnot(inherits(x,"VPTimeSeries"))
  args <- list(...)
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

  #plot the image
  image.plot(x$dates,x$heights,plotdata,col=plot.colors,xlab=xlab,ylab=ylab,axis.args=axis.args,breaks=breaks,zlim=zlim,main=main,...)

  # overlay speed barbs
  if(barbs){
    if("xlim" %in% names(args)){
      stepsize.t=(max(args$xlim)-min(args$xlim))/barbs.t
      t.barbs=seq(min(args$xlim)+stepsize.t/2, max(args$xlim)-stepsize.t/2,length.out=barbs.t)
    }
    else{
      stepsize.t=(max(x$dates)-min(x$dates))/barbs.t
      t.barbs=seq(x$dates[1]+stepsize.t/2, tail(x$dates,1)-stepsize.t/2,length.out=barbs.t)
    }
    if("ylim" %in% names(args)){
      stepsize.h=(max(args$ylim)-min(args$ylim))/barbs.h
      h.barbs=seq(x$heights[1]+stepsize.h/2, tail(x$heights,1)-stepsize.h/2,length.out=barbs.h)
    }
    else{
      stepsize.h=(max(x$heights)-min(x$heights))/barbs.h
      h.barbs=seq(x$heights[1]+stepsize.h/2, tail(x$heights,1)-stepsize.h/2,length.out=barbs.h)
    }
    barbdata=expand.grid(date=t.barbs,height=h.barbs)
    barbdata$indext=sapply(barbdata$date,function(y) which.min(abs(x$dates - y)))
    barbdata$indexh=sapply(barbdata$height,function(y) which.min(abs(x$heights - y)))
    barbdata$ff=mapply(function(xx,yy) x$data$ff[xx,yy],barbdata$indexh,barbdata$indext)
    barbdata$dd=mapply(function(xx,yy) x$data$dd[xx,yy],barbdata$indexh,barbdata$indext)
    barbdata$dens=mapply(function(xx,yy) x$data$dens[xx,yy],barbdata$indexh,barbdata$indext)
    barbdata=barbdata[barbdata$dens>barbs.dens,]
    plot_wind_barbs(barbdata$date,barbdata$height,180+barbdata$dd,2*barbdata$ff,cex=0.7)
  }
}

plot_wind_barbs = function(cx, cy, direction = 0, speed = NA, fill = rep(0, length(cx)), circle = FALSE, cex = 1, col = "black")
{
  ### press is actually height in upper air ###
  ns = length(cx)
  if (length(cy) != ns) stop("X AND Y COORDINATES SHOULD HAVE SAME LENGTH!")

  msg = "ALL VARIABLES SHOULD HAVE SAME LENGTH AS COORDINATES, OR BE MISSING!!!"
  if (ns > 1) {
    if (length(direction) == 1) if (!is.na(direction)) stop(msg)
    if (length(speed) == 1) if (!is.na(speed)) stop(msg)
    if (length(fill) == 1) if (!is.na(fill)) stop(msg)
    if (length(direction) > 1 & length(direction) != ns) stop(msg)
    if (length(speed) > 1 & length(speed) != ns) stop(msg)
    if (length(fill) > 1 & length(fill) != ns) stop(msg)
  }

  tpar = par()
  size = tpar$csi
  scalex = (tpar$usr[2] - tpar$usr[1]) / tpar$pin[1]
  scaley = (tpar$usr[4] - tpar$usr[3]) / tpar$pin[2]
  scalex = (cex * (scalex * size)) / 5
  scaley = (cex * (scaley * size)) / 5
  for (i in 1 : ns) {
    x = cx[i]
    y = cy[i]
    if (is.na(x) | is.na(y)) next
    spd = speed[i]

    if (circle) {
      ts = seq(0, 2 * pi, length.out = 200)
      RX = sin(ts) * scalex
      X1 = RX + x
      RY = cos(ts) * scaley
      Y1 = RY + y
      if (!is.na(spd)) {
        if (spd == 0) {
          lines(RX * 2 + x, RY * 2 + y, col = col)
        }
      }
      if (fill[i] > 0) {
        lim = c(51, 101, 151, 200)
        polygon(c(x, X1[1 : lim[fill[i]]]), c(y, Y1[1 : lim[fill[i]]]), density = -1, col = col)
      }
      lines(RX + x, RY + y, col = col)
    } #end of circle

    if (!is.na(spd)) {
      if (spd > 0) {
        X1 = 0
        X2 = 0
        Y1 = 0
        Y2 = 5
        if (spd >= 5 & spd < 10) {
          X1 = c(X1, 0)
          X2 = c(X2, 1)
          Y1 = c(Y1, 5)
          Y2 = c(Y2, 5)
        }
        if (spd >= 10 & spd < 15) {
          X1 = c(X1, 0)
          X2 = c(X2, 2)
          Y1 = c(Y1, 5)
          Y2 = c(Y2, 5)
        }
        if (spd >= 15 & spd < 20) {
          X1 = c(X1, 0, 0)
          X2 = c(X2, 1, 2)
          Y1 = c(Y1, 4, 5)
          Y2 = c(Y2, 4, 5)
        }
        if (spd >= 20 & spd < 25) {
          X1 = c(X1, 0, 0)
          X2 = c(X2, 2, 2)
          Y1 = c(Y1, 4, 5)
          Y2 = c(Y2, 4, 5)
        }
        if (spd >= 25 & spd < 30) {
          X1 = c(X1, 0, 0, 0)
          X2 = c(X2, 1, 2, 2)
          Y1 = c(Y1, 3, 4, 5)
          Y2 = c(Y2, 3, 4, 5)
        }
        if (spd >= 30 & spd < 35) {
          X1 = c(X1, 0, 0, 0)
          X2 = c(X2, 2, 2, 2)
          Y1 = c(Y1, 3, 4, 5)
          Y2 = c(Y2, 3, 4, 5)
        }
        if (spd >= 35 & spd < 40) {
          X1 = c(X1, 0, 0, 0, 0)
          X2 = c(X2, 1, 2, 2, 2)
          Y1 = c(Y1, 2, 3, 4, 5)
          Y2 = c(Y2, 2, 3, 4, 5)
        }
        if (spd >= 40 & spd < 45) {
          X1 = c(X1, 0, 0, 0, 0)
          X2 = c(X2, 2, 2, 2, 2)
          Y1 = c(Y1, 2, 3, 4, 5)
          Y2 = c(Y2, 2, 3, 4, 5)
        }
        if (spd >= 45 & spd < 50) {
          X1 = c(X1, 0, 0, 0, 0, 0)
          X2 = c(X2, 1, 2, 2, 2, 2)
          Y1 = c(Y1, 1, 2, 3, 4, 5)
          Y2 = c(Y2, 1, 2, 3, 4, 5)
        }
        if (spd >= 50 & spd < 55) {
          X1 = c(X1, 0, 0)
          X2 = c(X2, 2, 2)
          Y1 = c(Y1, 4, 5)
          Y2 = c(Y2, 4.5, 4.5)
        }
        if (spd >= 55 & spd < 60) {
          X1 = c(X1, 0, 0, 0)
          X2 = c(X2, 1, 2, 2)
          Y1 = c(Y1, 3, 4, 5)
          Y2 = c(Y2, 3, 4.5, 4.5)
        }
        if (spd >= 60 & spd < 65) {
          X1 = c(X1, 0, 0, 0)
          X2 = c(X2, 2, 2, 2)
          Y1 = c(Y1, 3, 4, 5)
          Y2 = c(Y2, 3, 4.5, 4.5)
        }
        dir = (direction[i] / 360) * 2 * pi
        rot = cbind(c(cos(dir), -sin(dir)), c(sin(dir), cos(dir)))
        S1 = rbind(X1, Y1)
        S2 = rbind(X2, Y2)
        S1 = rot %*% S1
        S2 = rot %*% S2
        S1 = S1 * c(scalex, scaley) + c(x, y)
        S2 = S2 * c(scalex, scaley) + c(x, y)
      }
      if (spd > 0) {
        segments(S1[1, ], S1[2, ], S2[1, ], S2[2, ], col = col, lwd = 1)
      }
    } #end of (!is.na(spd))
  } #end of ns
  invisible()
}
