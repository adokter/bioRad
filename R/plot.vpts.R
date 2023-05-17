#' Plot a time series of vertical profiles (`vpts`)
#'
#' Plot a time series of vertical profiles  of class `vpts`.
#'
#' @param x A vp class object inheriting from class `vpts`.
#' @param xlab A title for the x-axis.
#' @param ylab A title for the y-axis.
#' @param quantity Character string with the quantity to plot,
#' one of '`dens`','`eta`','`dbz`','`DBZH`' for density,
#' reflectivity, reflectivity factor and total reflectivity factor,
#' respectively.
#' @param log Logical, whether to display `quantity` data on a
#' logarithmic scale.
#' @param barbs Logical, whether to overlay speed barbs.
#' @param barbs_height Integer, number of barbs to plot in altitudinal dimension.
#' @param barbs_time Integer, number of barbs to plot in temporal dimension.
#' @param barbs_dens_min Numeric, lower threshold in aerial density of individuals
#' for plotting speed barbs in individuals/km^3.
#' @param zlim Optional numerical atomic vector of length 2, specifying the
#' range of `quantity` values to plot.
#' @param legend_ticks Numeric atomic vector specifying the ticks on the
#' color bar.
#' @param main A title for the plot.
#' @param na_color Color to use for NA values, see class [`vpts()`][summary.vpts] conventions.
#' @param nan_color Color to use for NaN values, see class [`vpts()`][summary.vpts] conventions.
#' @param n_color The number of colors (>=1) to be in the palette.
#' @param palette (Optional) character vector of hexadecimal color values defining
#' the plot color scale, e.g. output from [viridis][viridisLite::viridis]
#' @param ... Additional arguments to be passed to the low level
#' [image][graphics::image] plotting function.
#' @param barbs.h Deprecated argument, use barbs_height instead.
#' @param barbs.t Deprecated argument, use barbs_time instead.
#' @param barbs.dens Deprecated argument, use barbs_dens_min instead.
#' @param legend.ticks Deprecated argument, use legend_ticks instead.
#'
#' @method plot vpts
#'
#' @export
#'
#' @details Aerial abundances can be visualized in four related quantities, as specified
#' by argument `quantity`:
#' * `dens`: the aerial density of individuals. This quantity is dependent on
#'   the assumed radar cross section (RCS) in the `x$attributes$how$rcs_bird`
#'   attribute
#' * `eta`: reflectivity. This quantity is independent of the value of the
#'   `rcs_bird` attribute
#' * `dbz`: reflectivity factor. This quantity is independent of the value of
#'   the `rcs_bird` attribute, and corresponds to the dBZ scale commonly used in
#'   weather radar meteorology. Bioscatter by birds tends to occur at much
#'   higher reflectivity factors at S-band than at C-band
#' * `DBZH`: total reflectivity factor. This quantity equals the reflectivity
#'   factor of all scatterers (biological and meteorological scattering
#'   combined)
#'
#' Aerial velocities can be visualized in three related quantities, as specified
#' by argument `quantity`:
#' * `ff`: ground speed. The aerial velocity relative to the ground surface in
#'   m/s.
#' * `u`: eastward ground speed component in m/s.
#' * `v`: northward ground speed component in m/s.
#'
#' ## barbs
#' In the speed barbs, each half flag represents 2.5 m/s, each full flag 5 m/s,
#' each pennant (triangle) 25 m/s
#'
#' ## legend_ticks / zlim
#' Default legend ticks and plotting range are specified based on `quantity`,
#' radar wavelength (S- vs C-band), and value of `log`
#'
#' ## log
#' Quantities `u` and `v` cannot be plotted on a logarithmic scale, because
#' these quantities assume negative values.  For quantities `DBZH` and `dbz`
#' `log=TRUE` is ignored, because these quantities are already logarithmic.
#'
#' @examples
#' # locate example file:
#' ts <- example_vpts
#' # plot density of individuals for the first 500 time steps, in the altitude
#' # layer 0-3000 m.
#' plot(ts[1:500], ylim = c(0, 3000))
#' # plot total reflectivity factor (rain, birds, insects together):
#' plot(ts[1:500], ylim = c(0, 3000), quantity = "DBZH")
#' # regularize the time grid, which includes empty (NA) profiles at
#' # time steps without data:
#' ts_regular <- regularize_vpts(ts)
#' plot(ts_regular)
#' # change the color of missing NA data to red
#' plot(ts_regular, na_color="red")
#' # change the color palette:
#' plot(ts_regular[1:1000], ylim = c(0, 3000), palette=viridis::viridis(1000))
#' # change and inverse the color palette:
#' plot(ts_regular[1:1000], ylim = c(0, 3000), palette=rev(viridis::viridis(1000, option="A")))
#' # plot the speed profile:
#' plot(ts_regular[1:1000], quantity="ff")
#' # plot the northward speed component:
#' plot(ts_regular[1:1000], quantity="v")
#' # plot speed profile with more legend ticks,
#' plot(ts_regular[1:1000], quantity="ff", legend_ticks=seq(0,20,2), zlim=c(0,20))
plot.vpts <- function(x, xlab = "time", ylab = "height [m]", quantity = "dens",
                      log = NA, barbs = TRUE, barbs_height = 10,
                      barbs_time = 20, barbs_dens_min = 5,
                      zlim, legend_ticks, legend.ticks, main,
                      barbs.h = 10, barbs.t = 20, barbs.dens = 5,
                      na_color = "#C8C8C8", nan_color = "white",
                      n_color=1000, palette = NA,
                      ...) {
  stopifnot(inherits(x, "vpts"))
  stopifnot(quantity %in% names(x$data))

  if (hasArg("param")) stop("unknown function argument 'param`. Did you mean `quantity`?")

  # deprecate function arguments
  if (!missing(barbs.h)) {
    warning("argument barbs.h is deprecated; please use barbs_height instead.",
      call. = FALSE
    )
    barbs_height <- barbs.h
  }
  if (!missing(barbs.t)) {
    warning("argument barbs.t is deprecated; please use barbs_time instead.",
      call. = FALSE
    )
    barbs_time <- barbs.t
  }
  if (!missing(barbs.dens)) {
    warning("argument barbs.dens is deprecated; please use barbs_dens_min ",
      "instead.",
      call. = FALSE
    )
    barbs_dens_min <- barbs.dens
  }
  if (!missing(legend.ticks)) {
    warning("argument legend.ticks is deprecated; please use legend_ticks ",
      "instead.",
      call. = FALSE
    )
    legend_ticks <- legend.ticks
  }

  args <- list(...)
  if (!x$regular) {
    warning(
      "Irregular time-series: missing profiles will not be visible.",
      " Use 'regularize_vpts' to make time series regular."
    )
  }

  if(are_equal(log, NA)){
    if(quantity %in% c("dens","eta")){
      log = TRUE
    }
    else{
      log = FALSE
    }
  }
  assert_that(is.flag(log))

  assert_that(is.count(n_color))
  assert_that(is.string(na_color))
  if(!missing(nan_color)) assert_that(is.string(nan_color))

  if (!missing(zlim)) {
    assert_that(is.numeric(zlim), length(zlim) == 2, zlim[2] > zlim[1])
    if (log && !(quantity %in% c("DBZH", "dbz"))) {
      assert_that(zlim[1] > 0, msg = "zlim[1] not greater than 0. Positive values expected for zlim when argument 'log' is TRUE. Run ?plot.vpts for details.")
    }
  }

  # remove profiles with duplicate timestamps:
  index_duplicates <- which(x$timesteps == 0) + 1
  if (length(index_duplicates) > 0) {
    warning(paste("Dropped", length(index_duplicates), "profiles with duplicate datetime values"))
    x <- x[-index_duplicates]
  }

  # prepare zlim, ticks and legendticks
  if (missing(zlim)) {
    # first define defaults:
    finite_data <- (x$data[[quantity]][is.finite(x$data[[quantity]])])
    zlim = c(min(finite_data),max(finite_data))
    legendticks <- ticks <- seq(zlim[1], zlim[2], length.out = 10)
    # specific quantities:
    if (quantity == "dens" & log) {
      ticks <- legendticks <- c(1, 2, 5, 10, 25, 50, 100, 200, 500, 1000)
      zlim <- c(.5, 1000)
    }
    if (quantity == "dens" & !log) {
      ticks <- legendticks <- seq(0, 500, 20)
      zlim <- c(0, 500)
    }
    if (quantity == "eta" & log) {
      ticks <- legendticks <- 10 * c(1, 2, 5, 10, 25, 50, 100, 200, 500, 1000)
      zlim <- c(5, 10000)
    }
    if (quantity == "eta" & !log) {
      ticks <- legendticks <- seq(0, 5000, 500)
      zlim <- c(0, 5000)
    }
    if (quantity == "dbz" || quantity == "DBZH") {
      if (x$attributes$how$wavelength >= 10) {
        ticks <- legendticks <- seq(-5, 30, 5)
        zlim <- c(-5, 30)
      }
      else {
        ticks <- legendticks <- seq(-20, 10, 5)
        zlim <- c(-20, 10)
      }
    }
    if (quantity %in% c('u','v')) {
      ticks <- legendticks <- seq(-20, 20, 5)
      zlim <- c(-20, 20)
    }
    if (quantity == "ff" & log) {
      ticks <- legendticks <- c(1, 2, 5, 10, 20)
      zlim <- c(1, 20)
    }
    if (quantity == "ff" & !log) {
      ticks <- legendticks <- seq(0, 20, 5)
      zlim <- c(0, 20)
    }
    if (quantity %in% c("dd","heading") & !log){
      ticks <- legendticks <- seq(0,360,45)
      zlim <- c(0,360)
    }

  } else {
    ticks <- legendticks <- seq(zlim[1], zlim[2], length.out = 10)
  }
  if (!missing(legend_ticks)) {
    ticks <- legendticks <- legend_ticks
  }

  # set up the plot labels
  if (missing(main)) {
    main <- switch(quantity,
                   "dens" = expression("volume density [#/km"^3 * "]"),
                   "eta" = expression("reflectivity " * eta * " [cm"^2 * "/km"^3 * "]"),
                   "dbz" = expression("reflectivity factor [dBZ"[e] * "]"),
                   "DBZH" = expression("total reflectivity factor [dBZ"[e] * "]"),
                   "ff" = expression("ground speed [m/s]"),
                   "dd" = expression("ground speed direction [deg]"),
                   "u" = expression("eastward ground speed component u [m/s]"),
                   "v" = expression("northward ground speed component v [m/s]"),
                   quantity
    )
  }

  # extract the data from the time series object
  plotdata <- t(get_quantity(x, quantity))

  # check if we should ignore log argument
  if (log & quantity %in% c("dbz","DBZH","u","v")) {
    if (!missing(log)) {
      if(quantity %in% c("dbz","DBZH")){
        warning(
          paste("Reflectivity factor",quantity,"is already logarithmic,",
                "ignoring 'log' argument...")
        )
      } else{
        warning(
          paste("Velocity",quantity,"has negative values and can't be plotted on a log scale,",
                "ignoring 'log' argument...")
        )
      }
    }
    log <- FALSE
  }

  # do log-transformations:
  if (log) {
    plotdata <- log(plotdata)
    legendticks <- log(ticks)
    zlim <- log(zlim)
  }

  # set color scales and (palettes
  if (!are_equal(palette, NA)) {
    if(!(is.character(palette) && length(palette) > 1)) stop("palette should be a character vector with hex color values")
  }
  else{
    if(quantity %in% c("dens","eta","dbz","DBZH")){
      palette <- colorRampPalette(colors = vpts_default_palette,alpha = TRUE)(n_color)
    } else if(quantity %in% c("u","v")){
      palette <- rev(color_palette("VRADH", n_color=n_color))
    } else if(quantity %in% c("dd","heading")){
      palette <- c(rev(viridis::magma(n_color/2)),viridis::magma(n_color/2))
    } else{
      palette <- rev(viridis::magma(n_color))
    }
  }

  # add NA and NaN colors add beginning of palette
  palette_na_nan <- c(na_color, nan_color, palette)

  zstep <- (zlim[2] - zlim[1]) / length(palette);
  breaks <- seq(zlim[1]-2*zstep, zlim[2], length.out = length(palette_na_nan)+1)

  # if a regular time series, use the regular timegrid for plotting
  # (in case keep_datetime = TRUE option is used in regularize_vpts())
  if(x$regular) x$datetime <- seq(from = x$daterange[1], to = x$daterange[2], by = x$timesteps[1])

  # move points out of zlim range into valid color range
  plotdata[plotdata < zlim[1]] <- zlim[1]
  plotdata[plotdata > zlim[2]] <- zlim[2]
  # set NA and NaN values
  plotdata[is.na2(plotdata)] <- zlim[1]-1.5*zstep
  plotdata[is.nan(plotdata)] <- zlim[1]-0.5*zstep

  stopifnot(!is.null(interval <- x$attributes$where$interval))
  axis.args <- list(at = legendticks, labels = ticks)

  # plot the image
  image.plot(x$datetime, x$height + interval / 2, plotdata,
    col = palette_na_nan, xlab = xlab,
    ylab = ylab, axis.args = axis.args, breaks = breaks,
    zlim = c(zlim[1]-2*zstep,zlim[2]), main = main, ...
  )

  # overlay speed barbs
  if (barbs) {
    if ("xlim" %in% names(args)) {
      t.barbs <- seq(min(args$xlim), max(args$xlim), length.out = barbs_time)
    } else {
      t.barbs <- seq(x$datetime[1], tail(x$datetime, 1), length.out = barbs_time)
    }
    if ("ylim" %in% names(args)) {
      h.barbs <- seq(min(args$ylim), max(args$ylim), length.out = barbs_height + 1)
    } else {
      h.barbs <- seq(x$height[1], tail(x$height, 1) + interval, length.out = barbs_height + 1)
    }
    h.barbs <- h.barbs[-length(h.barbs)] + diff(h.barbs) / 2
    barbdata <- expand.grid(date = t.barbs, height = h.barbs)
    barbdata$indext <- sapply(
      barbdata$date,
      function(y) which.min(abs(x$datetime - y))
    )
    barbdata$indexh <- sapply(
      barbdata$height,
      function(y) which.min(abs(x$height + interval / 2 - y))
    )
    barbdata$ff <- mapply(
      function(xx, yy) x$data$ff[xx, yy], barbdata$indexh, barbdata$indext
    )
    barbdata$dd <- mapply(
      function(xx, yy) x$data$dd[xx, yy], barbdata$indexh, barbdata$indext
    )
    barbdata$dens <- mapply(
      function(xx, yy) x$data$dens[xx, yy], barbdata$indexh, barbdata$indext
    )
    barbdata <- barbdata[barbdata$dens > barbs_dens_min, ]
    plot_wind_barbs(barbdata$date, barbdata$height, 180 + barbdata$dd,
      barbdata$ff,
      cex = 0.7
    )
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

  msg <- "ALL VARIABLES SHOULD HAVE SAME LENGTH AS COORDINATES, OR BE MISSING!!!"
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
    x <- cx[i]
    y <- cy[i]
    if (is.na(is.na(x) || is.na(y))) next
    if (is.na(x) || is.na(y)) next
    spd <- speed[i]

    if (circle) {
      ts <- seq(0, 2 * pi, length.out = 200)
      RX <- sin(ts) * scalex
      X1 <- RX + x
      RY <- cos(ts) * scaley
      Y1 <- RY + y
      if (!is.na(spd)) {
        if (spd == 0) {
          lines(RX * 2 + x, RY * 2 + y, col = col)
        }
      }
      if (fill[i] > 0) {
        lim <- c(51, 101, 151, 200)
        polygon(c(as.numeric(x), X1[1:lim[fill[i]]]), c(y, Y1[1:lim[fill[i]]]),
          density = -1, col = col
        )
      }
      lines(RX + x, RY + y, col = col)
    } # end of circle

    if (!is.na(spd)) {
      if (spd > 0) {
        X1 <- 0
        X2 <- 0
        Y1 <- 0
        Y2 <- 5

        if (spd >= 1.25 & spd < 3.75) {
          X1 <- c(X1, 0)
          X2 <- c(X2, 1)
          Y1 <- c(Y1, 5)
          Y2 <- c(Y2, 5)
        }
        if (spd >= 3.75 & spd < 6.25) {
          X1 <- c(X1, 0)
          X2 <- c(X2, 2)
          Y1 <- c(Y1, 5)
          Y2 <- c(Y2, 5)
        }
        if (spd >= 6.25 & spd < 8.75) {
          X1 <- c(X1, 0, 0)
          X2 <- c(X2, 1, 2)
          Y1 <- c(Y1, 4, 5)
          Y2 <- c(Y2, 4, 5)
        }
        if (spd >= 8.75 & spd < 11.25) {
          X1 <- c(X1, 0, 0)
          X2 <- c(X2, 2, 2)
          Y1 <- c(Y1, 4, 5)
          Y2 <- c(Y2, 4, 5)
        }
        if (spd >= 11.25 & spd < 13.75) {
          X1 <- c(X1, 0, 0, 0)
          X2 <- c(X2, 1, 2, 2)
          Y1 <- c(Y1, 3, 4, 5)
          Y2 <- c(Y2, 3, 4, 5)
        }
        if (spd >= 13.75 & spd < 16.25) {
          X1 <- c(X1, 0, 0, 0)
          X2 <- c(X2, 2, 2, 2)
          Y1 <- c(Y1, 3, 4, 5)
          Y2 <- c(Y2, 3, 4, 5)
        }
        if (spd >= 16.25 & spd < 18.75) {
          X1 <- c(X1, 0, 0, 0, 0)
          X2 <- c(X2, 1, 2, 2, 2)
          Y1 <- c(Y1, 2, 3, 4, 5)
          Y2 <- c(Y2, 2, 3, 4, 5)
        }
        if (spd >= 18.75 & spd < 21.25) {
          X1 <- c(X1, 0, 0, 0, 0)
          X2 <- c(X2, 2, 2, 2, 2)
          Y1 <- c(Y1, 2, 3, 4, 5)
          Y2 <- c(Y2, 2, 3, 4, 5)
        }
        if (spd >= 21.25 & spd < 23.75) {
          X1 <- c(X1, 0, 0, 0, 0, 0)
          X2 <- c(X2, 1, 2, 2, 2, 2)
          Y1 <- c(Y1, 1, 2, 3, 4, 5)
          Y2 <- c(Y2, 1, 2, 3, 4, 5)
        }
        if (spd >= 23.75 & spd < 26.25) {
          X1 <- c(X1, 0, 0)
          X2 <- c(X2, 2, 2)
          Y1 <- c(Y1, 4, 5)
          Y2 <- c(Y2, 4.5, 4.5)
        }
        if (spd >= 26.25 & spd < 28.75) {
          X1 <- c(X1, 0, 0, 0)
          X2 <- c(X2, 1, 2, 2)
          Y1 <- c(Y1, 3, 4, 5)
          Y2 <- c(Y2, 3, 4.5, 4.5)
        }
        if (spd >= 28.75 & spd < 31.25) {
          X1 <- c(X1, 0, 0, 0)
          X2 <- c(X2, 2, 2, 2)
          Y1 <- c(Y1, 3, 4, 5)
          Y2 <- c(Y2, 3, 4.5, 4.5)
        }
        dir <- (direction[i] / 360) * 2 * pi
        rot <- cbind(c(cos(dir), -sin(dir)), c(sin(dir), cos(dir)))
        S1 <- rbind(X1, Y1)
        S2 <- rbind(X2, Y2)
        S1 <- rot %*% S1
        S2 <- rot %*% S2
        S1 <- S1 * c(scalex, scaley) + c(as.numeric(x), y)
        S2 <- S2 * c(scalex, scaley) + c(as.numeric(x), y)
      }
      if (spd > 0) {
        segments(S1[1, ], S1[2, ], S2[1, ], S2[2, ], col = col, lwd = 1)
      }
    } # end of (!is.na(spd))
  } # end of ns
  invisible()
}

# true when NA but not when NaN
is.na2 <- function(x) {
  is.na(x) & !is.nan(x)
}
