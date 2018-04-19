#' Plot a time series of vertical profiles (\code{vpts})
#'
#' Plot a time series of vertical profiles  of class \code{vpts}.
#'
#' @param x A vp class object inheriting from class \code{vpts}.
#' @param xlab A title for the x-axis.
#' @param ylab A title for the y-axis.
#' @param quantity Character string with the quantity to plot,
#' one of '\code{dens}','\code{eta}','\code{dbz}','\code{DBZH}' for density,
#' reflectivity, reflectivity factor and total reflectivity factor,
#' respectively.
#' @param log Logical, whether to display \code{quantity} data on a
#' logarithmic scale.
#' @param barbs Logical, whether to overlay speed barbs.
#' @param barbs.h Integer, number of barbs to plot in altitudinal dimension.
#' @param barbs.t Integer, number of barbs to plot in temporal dimension.
#' @param barbs.dens Numeric, lower threshold in aerial density of individuals
#' for plotting speed barbs in individuals/km^3.
#' @param zlim Optional numerical atomic vector of length 2, specifying the
#' range of \code{quantity} values to plot.
#' @param legend.ticks Numeric atomic vector specifying the ticks on the
#' color bar.
#' @param main A title for the plot.
#' @param ... Additional arguments to be passed to the low level
#' \link[graphics]{image} plotting function.
#'
#' @method plot vpts
#'
#' @export
#'
#' @details
#' Profile can be visualised in three related quantities, as specified
#' by argument \code{quantity}:
#' \describe{
#'  \item{"\code{dens}"}{the aerial density of individuals. This quantity is
#'    dependent on the assumed radar cross section (RCS) in the
#'    \code{x$attributes$how$rcs_bird} attribute}
#'  \item{"\code{eta}"}{reflectivity. This quantity is independent of the
#'    value of the \code{rcs_bird} attribute}
#'  \item{"\code{dbz}"}{reflectivity factor. This quantity is independent
#'    of the value of the \code{rcs_bird} attribute, and corresponds to the
#'    dBZ scale commonly used in weather radar meteorology. Bioscatter by birds
#'    tends to occur at much higher reflectivity factors at S-band
#'    than at C-band}
#'  \item{"\code{DBZH}"}{total reflectivity factor. This quantity equals the
#'    reflectivity factor of all scatterers (biological and meteorological
#'    scattering cambined)}
#' }
#' In the speed barbs, each half flag represents 2.5 m/s, each full flag 5 m/s,
#' each pennant (triangle) 25 m/s
#'
#' @examples
#' # locate example file:
#' VPtable <- system.file("extdata", "VPtable.txt", package = "bioRad")
#' # load and regularize time series of vertical profiles:
#' ts <- regularize(read_vpts(VPtable, radar = "KBGM", wavelength = 'S'))
#' # plot density of individuals for the first 500 time steps, in the altitude
#' # layer 0-3000 m.
#' plot(ts[1:500], ylim = c(0, 3000))
#' # plot total reflectivity factor (rain, birds, insects together):
#' plot(ts[1:500], ylim = c(0, 3000), quantity = "DBZH")
plot.vpts <- function(x, xlab = "time", ylab = "height [m]", quantity = "dens",
                      log = TRUE, barbs = TRUE, barbs.h = 10, barbs.t = 20,
                      barbs.dens = 5, zlim, legend.ticks, main, ...) {
  stopifnot(inherits(x, "vpts"))
  stopifnot(quantity %in% c("dens", "eta", "dbz", "DBZH"))
  args <- list(...)
  if (!x$regular) {
    warning("Irregular time-series: missing profiles will not be visible.",
            "Use 'regularize' to make time series regular.")
  }

  # prepare zlim, ticks and legendticks
  if (missing(zlim)) {
    if (quantity == "dens" & log) {
      ticks <- legendticks <- c(1, 2, 5, 10, 25, 50, 100, 200, 500, 1000)
      zlim <- c(.5, 1000)
    }
    if (quantity == "dens" & !log) {
      ticks <- legendticks <- seq(0, 500, 20)
      zlim <- c(0, 500)
    }
    if (quantity == "eta" & log) {
      ticks <- legendticks <- 10*c(1, 2, 5, 10, 25, 50, 100, 200, 500, 1000)
      zlim <- c(5,10000)
    }
    if (quantity == "eta" & !log) {
      ticks <- legendticks <- seq(0, 5000, 500)
      zlim <- c(0,5000)
    }
    if (quantity == "dbz" || quantity == "DBZH") {
      if (x$attributes$how$wavelength > 10) {
        ticks <- legendticks <- seq(-5, 30, 5)
        zlim <- c(-5, 30)
      }
      else{
        ticks <- legendticks <- seq(-20, 10, 5)
        zlim <- c(-20, 10)
      }
    }
  } else {
    ticks <- legendticks <- seq(zlim[1], zlim[2], length.out = 10)
  }
  if (!missing(legend.ticks)) {
    ticks <- legendticks <- legend.ticks
  }

  # set up the plot labels
  if (missing(main)) {
    if (quantity == "dens") main = expression("volume density [#/km"^3*"]")
    if (quantity == "eta") main = expression("reflectivity "*eta*" [cm"^2*"/km"^3*"]")
    if (quantity == "dbz") main = expression("reflectivity factor [dBZ"[e] * "]")
    if (quantity == "DBZH") main = expression("total reflectivity factor [dBZ"[e] * "]")
  }

  # extract the data from the time series object
  if (quantity == "dens") plotdata = t(get_quantity(x,quantity))
  if (quantity == "eta") plotdata = t(get_quantity(x,quantity))
  if (quantity == "dbz") {
    if (log) {
      if (!missing(log)) {
        warning("Reflectivity factor 'dbz' is already logarithmic,",
                "ignoring 'log' argument...")
      }
      log <- FALSE
    }
    plotdata <- t(get_quantity(x, quantity))
  }
  if (quantity == "DBZH") {
    if (log) {
      if (!missing(log)) {
        warning("Total reflectivity factor 'DBZH' is already logarithmic,",
                "ignoring 'log' argument...")
      }
      log <- FALSE
    }
    plotdata <- t(get_quantity(x,quantity))
  }

  # do log-transformations:
  if (log) {
    plotdata <- log(plotdata)
    legendticks <- log(ticks)
    zlim <- log(zlim)
  }
  breaks <- c(zlim[1] - (zlim[2] - zlim[1])/1000,
              seq(zlim[1], zlim[2], length.out = 256))
  # move points out of zlim range into valid color range
  plotdata[plotdata < (breaks[2] + breaks[3])/2] <- (breaks[2] + breaks[3])/2
  plotdata[plotdata > zlim[2]] <- breaks[length(breaks)]
  plotdata[is.na2(plotdata)] <- (breaks[1] + breaks[2])/2

  zlim[1] <- breaks[1]
  axis.args <- list(at = legendticks, labels = ticks)
  # FIXME: want to change this to
  # plotdata[is.nan(plotdata)]=(breaks[2]+breaks[3])/2
  # when calculate_vp stdout also differentiates between NA and NaN:
  plotdata[is.na(plotdata)] <- (breaks[2] + breaks[3])/2
  # FIXME: want to change this to
  # plotdata[is.na2(plotdata)]=(breaks[1]+breaks[2])/2
  # when calculate_vp stdout also differentiates between NA and NaN:
  plotdata[is.na(plotdata)] <- (breaks[2] + breaks[3])/2

  #plot the image
  image.plot(x$dates, x$heights, plotdata, col = plot_colors, xlab = xlab,
             ylab = ylab, axis.args = axis.args, breaks = breaks,
             zlim = zlim, main = main, ...)

  # overlay speed barbs
  if (barbs) {
    if ("xlim" %in% names(args)) {
      t.barbs <- seq(min(args$xlim), max(args$xlim), length.out = barbs.t)
    } else {
      t.barbs <- seq(x$dates[1], tail(x$dates, 1), length.out = barbs.t)
    }
    if ("ylim" %in% names(args)) {
      h.barbs <- seq(min(args$ylim), max(args$ylim), length.out = barbs.h)
    } else {
      h.barbs <- seq(x$heights[1], tail(x$heights, 1), length.out = barbs.h)
    }
    barbdata <- expand.grid(date = t.barbs, height = h.barbs)
    barbdata$indext <- sapply(barbdata$date,
                              function(y) which.min(abs(x$dates - y)))
    barbdata$indexh <- sapply(barbdata$height,
                              function(y) which.min(abs(x$heights - y)))
    barbdata$ff <- mapply(
      function(xx,yy) x$data$ff[xx,yy], barbdata$indexh, barbdata$indext)
    barbdata$dd <- mapply(
      function(xx,yy) x$data$dd[xx,yy], barbdata$indexh, barbdata$indext)
    barbdata$dens <- mapply(
      function(xx,yy) x$data$dens[xx,yy], barbdata$indexh, barbdata$indext)
    barbdata <- barbdata[barbdata$dens > barbs.dens,]
    plot_wind_barbs(barbdata$date,barbdata$height, 180 + barbdata$dd,
                    2*barbdata$ff, cex = 0.7)
  }
}

plot_wind_barbs <- function(cx, cy, direction = 0, speed = NA,
                            fill = rep(0, length(cx)), circle = FALSE,
                            cex = 1, col = "black") {
  ### press is actually height in upper air ###
  ns <- length(cx)
  if (length(cy) != ns) {
    stop("X AND Y COORDINATES SHOULD HAVE SAME LENGTH!")
  }

  msg = "ALL VARIABLES SHOULD HAVE SAME LENGTH AS COORDINATES, OR BE MISSING!!!"
  if (ns > 1) {
    if (length(direction) == 1) if (!is.na(direction)) stop(msg)
    if (length(speed) == 1) if (!is.na(speed)) stop(msg)
    if (length(fill) == 1) if (!is.na(fill)) stop(msg)
    if (length(direction) > 1 & length(direction) != ns) stop(msg)
    if (length(speed) > 1 & length(speed) != ns) stop(msg)
    if (length(fill) > 1 & length(fill) != ns) stop(msg)
  }

  tpar <- par()
  size <- tpar$csi
  scalex <- (tpar$usr[2] - tpar$usr[1]) / tpar$pin[1]
  scaley <- (tpar$usr[4] - tpar$usr[3]) / tpar$pin[2]
  scalex <- (cex * (scalex * size)) / 5
  scaley <- (cex * (scaley * size)) / 5
  for (i in 1:ns) {
    x = cx[i]
    y = cy[i]
    if (is.na(is.na(x) || is.na(y))) next
    if (is.na(x) || is.na(y)) next
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
        polygon(c(x, X1[1:lim[fill[i]]]), c(y, Y1[1:lim[fill[i]]]),
                density = -1, col = col)
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

# true when NA but not when NaN
is.na2 <- function(x) {
  is.na(x) & !is.nan(x)
}
