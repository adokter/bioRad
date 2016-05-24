#default plotting parameters
N.wind.barbs.t.def = 32
N.wind.barbs.h.def = 10
dBZ.thres.barbs.def = -10
min.height.def = 0
max.height.def = 4
radar.lat.def = 52.1
radar.lon.def = 5.1
dBZ.factor.def = 335.4
thres.stdev.def = 2
thres.precip.def= 0.99
sigmabird.def = 11
yrange.def = c(0, 100)
twilight.elev.def = -6
h.int.range.def = c(0.5, 4.0)
contour.range.def = c(0.8, 3.8)
dt.axis.profiles.def = 3600 * 4
dt.axis.densities.def = 3600 * 24

plotvol2bird<-function(data,tmin=min(data$Time),tmax=min(max(data$Time),min(data$Time)+7*24*3600),
                   pdfname="output.pdf",
                   N.wind.barbs.t=N.wind.barbs.t.def,
                   N.wind.barbs.h=N.wind.barbs.h.def,
                   dBZ.thres.barbs=dBZ.thres.barbs.def,
                   min.height=min.height.def,
                   max.height=max.height.def,
                   radar.lat=radar.lat.def,
                   radar.lon=radar.lon.def,
                   dBZ.factor=dBZ.factor.def,
                   thres.stdev=thres.stdev.def,
                   thres.stdev.upper=1000,
                   thres.precip=thres.precip.def,
                   sigmabird=sigmabird.def,
                   yrange=yrange.def,
                   twilight.elev=twilight.elev.def,
                   h.int.range=h.int.range.def,
                   contour.range=contour.range.def,
                   dt.axis.profiles=dt.axis.profiles.def,
                   dt.axis.densities=dt.axis.densities.def,
                   all.dBZ=F){
  #Determine times (defining time cells on x axis)
  t0=tmin
  t1=tmax
  x <- data[data$Time>t0 & data$Time<t1,]
  x$Heig=x$Heig/1000 # convert heights from m to km
  if(all.dBZ){
    x$dBZ=x$dBZAll
    title="vertical profile of reflectivity (VPR)"
  }
  else{
    title="vertical profile of birds (VPB)"
  }
  t = x$Time #specify the format of the timestamp in the dataset
  int.times = sort(t[(t >= t0) & (t <= t1)]) #specify the format of t0 and t1 written above
  ind.times = c(1, which(diff(int.times) > 0) + 1)
  times = int.times[ind.times]
  dt = min(diff(as.numeric(times)))
  dt = max(min(diff(as.numeric(times))),300)
  print(paste("min. time interval:",dt))
  missing.polygons.t = t0
  missing.polygons.h = NA
  ind.missing = which(diff(as.numeric(times)) > (1.5 * dt))
  if (length(ind.missing) > 0) {
    for (i in 1 : length(ind.missing)) {
      missing.polygons.t = c(missing.polygons.t, times[ind.missing[i]] + dt, times[ind.missing[i]] + dt, times[ind.missing[i]] + dt, times[ind.missing[i] + 1], times[ind.missing[i] + 1])
      missing.polygons.h = c(missing.polygons.h, NA, min.height, max.height, max.height, min.height)
    }
  }
  
  if (times[1] > t0) {
    times = c(t0, times)
    missing.polygons.t = c(missing.polygons.t, times[1], times[1], times[1], times[2], times[2])
    missing.polygons.h = c(missing.polygons.h, NA, min.height, max.height, max.height, min.height)
  }
  if (times[length(times)] < t1) {
    times = c(times, t1)
    missing.polygons.t = c(missing.polygons.t, times[length(times) - 1], times[length(times) - 1], times[length(times) - 1], times[length(times)], times[length(times)])
    missing.polygons.h = c(missing.polygons.h, NA, min.height, max.height, max.height, min.height)
  }
  N.times = length(times)
  
  #Determine height layers (define high cells on y axis)
  int.layers = sort(x$Heig)
  ind.layers = c(1, which(diff(int.layers) > 0) + 1)
  int.layers = int.layers[ind.layers]
  int.layers = int.layers[(int.layers >= min.height) & (int.layers <= max.height)]
  dh = min(diff(int.layers))
  layers = seq(min(int.layers), max(int.layers), by = dh)
  ind.layers = round((x$Heig - min(int.layers)) / dh) + 1
  N.layers = length(layers)
  
  #FILL data matrices (fill each cell with a matrix containing the reflectivity of birds and information about wind)
  int.cr = 10 * (contour.range - log10(dBZ.factor))
  height.integrated.dBZbird = seq(1, 1, length.out = N.times) * NA
  dBZbird = matrix(NA, nrow = N.layers, ncol = N.times)
  Ubird = dBZbird
  Vbird = dBZbird
  for (i in 1 : N.times) {
    ind.times = which((t == times[i]) & (x$StdDev > thres.stdev) & (x$StdDev < thres.stdev.upper))
    ind.h = which((x$Heig[ind.times] > h.int.range[1]) & (x$Heig[ind.times] < h.int.range[2]))
    if (length(ind.h) > 0) {
      height.integrated.dBZbird[i] = sum(10 ^ (x$dBZ[ind.times[ind.h]] / 10 + log10(dBZ.factor)), na.rm = TRUE) * dh / sigmabird
    } else {
      height.integrated.dBZbird[i] = NA
    }
    
    x$dBZ[ind.times[x$dBZ[ind.times] < int.cr[1]]] = int.cr[1]
    x$dBZ[ind.times[x$dBZ[ind.times] > int.cr[2]]] = int.cr[2]
    x$dBZ[ind.times[is.na(x$dBZ[ind.times])]] = int.cr[1] - int.cr[2]
    ind.h = which((x$Heig[ind.times] >= min.height) & (x$Heig[ind.times] <= max.height))
    dBZbird[ind.layers[ind.times[ind.h]], i] = x$dBZ[ind.times[ind.h]]
    Ubird[ind.layers[ind.times[ind.h]], i] = x$U[ind.times[ind.h]]
    Vbird[ind.layers[ind.times[ind.h]], i] = x$V[ind.times[ind.h]]
  }
  
  #Remap velocities
  int.t = seq(t0,t1, length.out = N.wind.barbs.t + 1)
  dt = (as.double(int.t[2]) - as.double(int.t[1])) / 2
  t.wind = int.t[1 : N.wind.barbs.t] + dt
  dh.barbs = (max.height - min.height) / (N.wind.barbs.h + 1)
  int.h = seq(dh.barbs * 0.5, (N.wind.barbs.h + 0.5) * dh.barbs, by = dh.barbs)
  h.wind = int.h[1 : N.wind.barbs.h] + dh.barbs / 2
  t.barbs = t0 - seq(1, 1, length.out = N.wind.barbs.h * N.wind.barbs.t)
  h.barbs = seq(0, 0, length.out = N.wind.barbs.h * N.wind.barbs.t)
  spd.barbs = h.barbs
  dir.barbs = h.barbs
  cnt = 0
  for (i in 1 : N.wind.barbs.t) {
    ind.t = which((times >= int.t[i]) & (times < int.t[i + 1]))
    if (length(ind.t) > 0) {
      for (j in 1 : N.wind.barbs.h) {
        ind.h = which((layers >= int.h[j]) & (layers < int.h[j + 1]))
        if (length(ind.h) > 0) {
          mz = mean(dBZbird[ind.h, ind.t], na.rm = TRUE)
          mu = mean(Ubird[ind.h, ind.t], na.rm = TRUE)
          mv = mean(Vbird[ind.h, ind.t], na.rm = TRUE)
          if (!(is.na(mu) | is.na(mv) | is.na(mz))) {
            if (mz >= dBZ.thres.barbs) {
              cnt = cnt + 1
              t.barbs[cnt] = t.wind[i]
              h.barbs[cnt] = h.wind[j]
              spd.barbs[cnt] = sqrt(mu ^ 2 + mv ^ 2)
              dir.barbs[cnt] = 270 - atan2(mv, mu) * 180 / pi
              if (dir.barbs[cnt] < 0) dir.barbs[cnt] = dir.barbs[cnt] + 360
            }
          }
        }
      }
    }
  }
  t.barbs = t.barbs[1 : cnt]
  h.barbs = h.barbs[1 : cnt]
  spd.barbs = spd.barbs[1 : cnt]
  dir.barbs = dir.barbs[1 : cnt]
  
  #Make color table
  r.points = c(1, 63, 82, 94, 146, 177, 192, 209, 256)
  r.values = c(255, 255, 163, 255, 255, 81, 81, 0, 0)
  g.points = c(1, 65, 80, 111, 143, 256)
  g.values = c(255, 255, 163, 163, 0, 0)
  b.points = c(1, 80, 97, 111, 128, 160, 207, 256)
  b.values = c(255, 0, 0, 82, 0, 0, 255, 0)
  plot.colors = rgb(c(200, approx(r.points, r.values, seq(1, 256, length.out = 255))$y), c(200, approx(g.points, g.values, seq(1, 256, length.out = 255))$y), c(200, approx(b.points, b.values, seq(1, 256, length.out = 255))$y), maxColorValue = 255)
  ct = seq(contour.range[1], contour.range[2], length.out = length(plot.colors))
  
  pdf(pdfname, width = 24 / 2.54, height = 16 / 2.54) #define the figure size
  par(omi = c(0, 0, 0, 0) / 2.54)
  layout(matrix(c(1, 2, 3, 4), 2, 2), widths = lcm(c(19.75, 4.25)), heights = lcm(c(7.4, 8.1)))
  
  #Plot profiles of bird densities, with wind barbs
  par(mai = c(1.1, 2, 0.5, 0.3) / 2.54)
  plot(times, seq(-1, -1, length.out = length(times)), ylim = c(0, max(layers)), xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  tick.times = as.POSIXct(seq(ceiling((as.numeric(times[1]) - as.numeric(as.POSIXct("20000101", format = "%Y%m%d"))) / dt.axis.profiles) * dt.axis.profiles, floor((as.numeric(times[length(times)]) - as.numeric(as.POSIXct("20000101", format = "%Y%m%d"))) / dt.axis.profiles) * dt.axis.profiles, by = dt.axis.profiles), origin = as.POSIXct("20000101", format = "%Y%m%d"))
  if (dt.axis.profiles < 86400) {
    axis.POSIXct(1, at = tick.times, format = "%H:%M")
  } else {
    axis.POSIXct(1, at = tick.times, format = "%d-%m")
  }
  axis(2, las = 2)
  mtext("time (UTC)", side = 1, line = 2)
  mtext("height (km)", side = 2, line = 2)
  mtext(paste(title,", ", format(times[1], format = "%d-%m-%Y %H:%M"), "  --  ", format(times[length(times)], format = "%Y-%m-%d %H:%M"), sep = ""), side = 3, line = 0.3)
  plot.dBZbird = 1 + round(254 * (t(dBZbird[, 1 : (length(times) - 1)] / 10 + log10(dBZ.factor)) - contour.range[1]) / (contour.range[2] - contour.range[1]))
  plot.dBZbird[plot.dBZbird < 1] = 0
  image(times, layers, plot.dBZbird, ylim = c(min.height, max.height), zlim = c(0, 255), col = plot.colors, add = TRUE)
  polygon(missing.polygons.t, missing.polygons.h, lty = 0, col = rgb(200, 200, 200, maxColorValue = 255))
  plot_wind_barbs(t.barbs, h.barbs, dir.barbs, spd.barbs * 3600 / 1852, cex = 0.7)
  grid.x = rep(c(times[1], times[1], times[length(times)]), floor(max.height) - ceiling(min.height) + 1)
  grid.y = rep(seq(ceiling(min.height), floor(max.height)), 3 * seq(1, 1, length.out = floor(max.height) - ceiling(min.height) + 1))
  grid.y[seq(1, length(grid.y), by = 3)] = NA
  lines(grid.x, grid.y, col = "black")
  grid.x = rep(tick.times, 3 * seq(1, 1, length.out = length(tick.times)))
  grid.y = rep(c(NA, min.height, max.height), length(tick.times))
  lines(grid.x, grid.y, col = "black")
  lines(c(times[1], times[1], times[length(times)], times[length(times)], times[1]), c(min.height, max.height, max.height, min.height, min.height), col = "black")
  
  #Plot height-integrated bird densities
  par(mai = c(1.1, 2, 1.2, 0.3) / 2.54)
  #dates = seq(as.POSIXct(paste(floor(t0 / 10000)), format = "%Y%m%d", tz = "UTC"), as.POSIXct(paste(floor(t1 / 10000)), format = "%Y%m%d", tz = "UTC"), by = 86400)
  dates = round(seq(t0-86400, t1, by = 86400),"days")
  dates.yyyymmdd = as.numeric(format(dates, format = "%Y%m%d"))
  twilight.rise = as.POSIXct(get_time_sun(lon = radar.lon, lat = radar.lat, yyyymmdd = dates.yyyymmdd, elev = 0, rise = TRUE) * 3600, origin = dates)
  twilight.set = as.POSIXct(get_time_sun(lon = radar.lon, lat = radar.lat, yyyymmdd = dates.yyyymmdd, elev = 0, rise = FALSE) * 3600, origin = dates)
  night.rise = as.POSIXct(get_time_sun(lon = radar.lon, lat = radar.lat, yyyymmdd = dates.yyyymmdd, elev = -6, rise = TRUE) * 3600, origin = dates)
  night.set = as.POSIXct(get_time_sun(lon = radar.lon, lat = radar.lat, yyyymmdd = dates.yyyymmdd, elev = -6, rise = FALSE) * 3600, origin = dates)
  
  twilight.polygons.t = dates[1]
  twilight.polygons.h = NA
  night.polygons.t = dates[1]
  night.polygons.h = NA
  date.lines.t = dates[1]
  date.lines.h = NA
  for (i in 1 : length(dates)) {
    if (twilight.rise[i] < twilight.set[i]) {
      twilight.polygons.t = c(twilight.polygons.t, dates[i], dates[i], dates[i], twilight.rise[i], twilight.rise[i], twilight.set[i], twilight.set[i], twilight.set[i], dates[i] + 86400, dates[i] + 86400)
      twilight.polygons.h = c(twilight.polygons.h, NA, yrange[1], yrange[2], yrange[2], yrange[1], NA, yrange[1], yrange[2], yrange[2], yrange[1])
    } else {
      twilight.polygons.t = c(twilight.polygons.t, twilight.set[i], twilight.set[i], twilight.set[i], twilight.rise[i], twilight.rise[i])
      twilight.polygons.h = c(twilight.polygons.h, NA, yrange[1], yrange[2], yrange[2], yrange[1])
    }
    
    if (night.rise[i] < night.set[i]) {
      night.polygons.t = c(night.polygons.t, dates[i], dates[i], dates[i], night.rise[i], night.rise[i], night.set[i], night.set[i], night.set[i], dates[i] + 86400, dates[i] + 86400)
      night.polygons.h = c(night.polygons.h, NA, yrange[1], yrange[2], yrange[2], yrange[1], NA, yrange[1], yrange[2], yrange[2], yrange[1])
    } else {
      night.polygons.t = c(night.polygons.t, night.set[i], night.set[i], night.set[i], night.rise[i], night.rise[i])
      night.polygons.h = c(night.polygons.h, NA, yrange[1], yrange[2], yrange[2], yrange[1])
    }
    
    date.lines.t = c(date.lines.t, dates[i], dates[i], dates[i])
    date.lines.h = c(date.lines.h, NA, yrange[1], yrange[2])
  }
  plot(times, seq(yrange[1] - yrange[2], yrange[1] - yrange[2], length.out = length(times)), ylim = yrange, xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  polygon(twilight.polygons.t, twilight.polygons.h, lty = 0, col = rgb(0.9, 0.9, 0.9))
  polygon(night.polygons.t, night.polygons.h, lty = 0, col = rgb(0.8, 0.8, 0.8))
  lines(date.lines.t, date.lines.h, col = "black")
  lines(times, height.integrated.dBZbird, col = "red")
  lines(c(times[1], times[1], times[length(times)], times[length(times)], times[1]), c(yrange[1], yrange[2], yrange[2], yrange[1], yrange[1]), col = "black")
  tick.times = as.POSIXct(seq(ceiling((as.numeric(times[1]) - as.numeric(as.POSIXct("20000101", format = "%Y%m%d"))) / dt.axis.densities) * dt.axis.densities, floor((as.numeric(times[length(times)]) - as.numeric(as.POSIXct("20000101", format = "%Y%m%d"))) / dt.axis.densities) * dt.axis.densities, by = dt.axis.densities), origin = as.POSIXct("20000101", format = "%Y%m%d"))
  if (dt.axis.densities < 86400) {
    axis.POSIXct(1, at = tick.times, format = "%H:%M")
  } else {
    axis.POSIXct(1, at = tick.times, format = "%d-%m")
  }
  axis(2, las = 2)
  grid.x = rep(tick.times, 3 * seq(1, 1, length.out = length(tick.times)))
  grid.y = rep(c(NA, yrange[1], yrange[2]), length(tick.times))
  lines(grid.x, grid.y, col = "black")
  mtext("time (UTC)", side = 1, line = 2)
  mtext(expression(Density ~ (Birds/km^{2})), side = 2, line = 3)
  if(all.dBZ){
    mtext("Height-integrated reflectivity", side = 3, line = 0.3)
  }
  else{
    mtext("Height-integrated bird density", side = 3, line = 0.3)
  }
  
  
  #Make color bar
  par(mai = c(1.1, 1.75, 0.5, 1.75) / 2.54)
  image(c(0, 0.5, 1), ct, rbind(ct[1 : (length(ct) - 1)], ct[1 : (length(ct) - 1)]), col = plot.colors[2 : length(plot.colors)], xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(c(0, 0, 1, 1, 0), c(min(ct), max(ct), max(ct), min(ct), min(ct)), col = "black")
  axis(2, at = seq(contour.range[1], contour.range[2], by = 0.5), las = 2)
  mtext("Reflectivity (log10)", side = 2, line = 3)
  axis(4, at = c(1, 2, 3), labels = c("1", "10", "100"), las = 2)
  mtext(expression(Density ~ (Birds/km^{3})), side = 4, line = 3)
  
  #End of figure
  dev.off()
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

get_time_sun = function(lon, lat, yyyymmdd, elev, rise = TRUE)
{
	#Convert date to julian day
	yyyy = floor(yyyymmdd / 10000)
	mm = floor((yyyymmdd - 10000 * yyyy) / 100)
	dd = yyyymmdd - 10000 * yyyy - 100 * mm
	jy=yyyy
	
	if (any(jy == 0)) write("get_time_sun: there is no year zero!", file = "")
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
	
	return(hour)
}