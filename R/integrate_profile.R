#' Vertically integrate profiles (\code{vp} or \code{vpts}) to an integrated profile (\code{vpi})
#'
#' Performs a vertical integration of density, reflectivity and migration
#' traffic rate, and a vertical averaging of ground speed and direction
#' weighted by density.
#'
#' @param x A \code{vp} or \code{vpts} object.
#' @param alt.min Minimum altitude in m.
#' @param alt.max Maximum altitude in m.
#' @param alpha Migratory direction in clockwise degrees from north.
#' @param interval.max Maximum time interval belonging to a single profile in
#' seconds. Traffic rates are set to zero at times \code{t} for which no
#' profiles can be found within the period \code{t-interval.max/2} to
#' \code{t+interval.max/2}. Ignored for single profiles of class \code{vp}.
#'
#' @return an object of class \code{vpi}, a data frame with vertically
#' integrated profile quantities
#'
#' @details
#' The function generates a specially classed data frame with the following
#' quantities:
#' \describe{
#'    \item{\code{datetime}}{POSIXct date of each profile in UTC}
#'    \item{\code{vid}}{Vertically Integrated Density in individuals/km^2.
#'       \code{vid} is a surface density, whereas \code{dens} in \code{vp}
#'       objects is a volume density.}
#'    \item{\code{vir}}{Vertically Integrated Reflectivity in cm^2/km^2}
#'    \item{\code{mtr}}{Migration Traffic Rate in individuals/km/h}
#'    \item{\code{rtr}}{Reflectivity Traffic Rate in cm^2/km/h}
#'    \item{\code{ff}}{Horizontal ground speed in m/s}
#'    \item{\code{dd}}{Horizontal ground speed direction in degrees}
#'    \item{\code{u}}{Ground speed component west to east in m/s}
#'    \item{\code{v}}{Ground speed component north to south in m/s}
#'    \item{\code{HGHT}}{Height above sea level in m}
#' }
#' Vertically integrated density and reflectivity are related according to
#' \eqn{vid=vir/rcs(x)}, with \link{rcs} the assumed radar cross section per
#' individual. Similarly, migration traffic rate and reflectivity traffic rate
#' are related according to \eqn{mtr=rtr/rcs(x)}
#'
#' See \link{mtr} for further information on the definition of migration
#' traffic rate.
#'
#' @export
#'
#' @examples
#' # MTR for a single vertical profile
#' integrate_profile(example_vp)
#'
#' # MTRs for a list of vertical profiles
#' integrate_profile(c(example_vp, example_vp))
#'
#' # MTRs for a time series of vertical profiles
#' # load example data:
#' data(example_vpts)
#' example_vpts
#' # print migration traffic rates
#' vpi <- integrate_profile(example_vpts)
#' # plot migration traffic rates for the full air column
#' plot(example_vpts)
#' # plot migration traffic rates for altitudes > 1 km above sea level
#' plot(integrate_profile(example_vpts, alt.min = 1000))
integrate_profile <- function(x, alt.min, alt.max,
                              alpha = NA, interval.max = Inf) {
  UseMethod("integrate_profile", x)
}

#' @describeIn integrate_profile Vertically integrate a vertical profile.
#'
#' @export
integrate_profile.vp <- function(x, alt.min = 0, alt.max = Inf, alpha = NA,
                                 interval.max = Inf) {
  stopifnot(inherits(x, "vp"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))
  stopifnot(is.na(alpha) || is.numeric(alpha))

  interval <- x$attributes$where$interval
  index <- which(x$data$HGHT >= alt.min & x$data$HGHT < alt.max)

  if (is.na(alpha)) {
    cosfactor <- rep(1, length(index))
  } else {
    cosfactor <- cos((get_quantity(x, "dd")[index] - alpha) * pi/180)
  }
  # multiply speeds by 3.6 to convert m/s to km/h
  mtr <- sum(get_quantity(x, "dens")[index] * cosfactor *
               get_quantity(x, "ff")[index] * 3.6 * interval/1000, na.rm = TRUE)
  rtr <- sum(get_quantity(x, "eta")[index] * cosfactor *
               get_quantity(x, "ff")[index] * 3.6 * interval/1000, na.rm = TRUE)
  vid <- sum(get_quantity(x, "dens")[index], na.rm = TRUE)*interval/1000
  vir <- sum(get_quantity(x, "eta")[index], na.rm = TRUE)*interval/1000
  height <- sum((x$heights[index] + x$attributes$where$interval/2) *
                  get_quantity(x, "dens")[index], na.rm = TRUE)/sum(
                    get_quantity(x,"dens")[index],na.rm = TRUE)
  u <- sum(get_quantity(x, "u")[index] * get_quantity(x, "dens")[index],
           na.rm = TRUE)/sum(get_quantity(x, "dens")[index], na.rm = TRUE)
  v <- sum(get_quantity(x,"v")[index] * get_quantity(x,"dens")[index],
           na.rm = TRUE)/sum(get_quantity(x, "dens")[index], na.rm = TRUE)
  ff <- sqrt(u^2 + v^2)
  dd <- (pi/2 - atan2(v, u)) * 180/pi
  # time-integrated measures not defined for a single profile:
  mt <- NA
  rt <- NA
  # prepare output
  output <- data.frame(datetime = x$datetime, mtr = mtr, vid = vid, vir = vir,
                       rtr = rtr, mt = mt, rt = rt, ff = ff, dd = dd, u = u,
                       v = v, HGHT = height)
  class(output) <- c("vpi", "data.frame")
  rownames(output) <- NULL
  attributes(output)$alt.min <- alt.min
  attributes(output)$alt.max <- alt.max
  attributes(output)$alpha <- alpha
  attributes(output)$rcs <- rcs(x)
  attributes(output)$lat <- x$attributes$where$lat
  attributes(output)$lon <- x$attributes$where$lon
  return(output)
}

#' @describeIn integrate_profile Vertically integrate a list of
#' vertical profiles.
#'
#' @export
integrate_profile.vplist <- function(x, alt.min = 0, alt.max = Inf,
                                     alpha = NA, interval.max = Inf) {
  stopifnot(inherits(x, "vplist"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))

  output <- do.call(rbind, lapply(x, integrate_profile.vp, alt.min = alt.min,
                                  alt.max = alt.max, alpha = alpha,
                                  interval.max = interval.max))
  class(output) <- c("vpi", "data.frame")
  attributes(output)$alt.min <- alt.min
  attributes(output)$alt.max <- alt.max
  attributes(output)$alpha <- alpha
  attributes(output)$rcs <- rcs(x)
  #TODO set lat/lon attributes
  return(output)
}

#' @describeIn integrate_profile Vertically integrate a time series of
#' vertical profiles.
#'
#' @export
integrate_profile.vpts <- function(x, alt.min = 0, alt.max = Inf,
                                   alpha = NA, interval.max = Inf) {
  stopifnot(inherits(x, "vpts"))
  stopifnot(is.numeric(alt.min) & is.numeric(alt.max))
  stopifnot(is.na(alpha) || is.numeric(alpha))

  interval <- x$attributes$where$interval
  index <- which(x$heights >= alt.min & x$heights < alt.max)
  if (is.na(alpha)) {
    cosfactor <- 1 + 0*get_quantity(x, "dd")[index,]
  } else {
    cosfactor <- cos((get_quantity(x, "dd")[index,] - alpha) * pi/180)
  }
  # multiply speeds by 3.6 to convert m/s to km/h
  mtr <- colSums(cosfactor * get_quantity(x, "ff")[index,] * 3.6 *
                   get_quantity(x, "dens")[index,],na.rm = TRUE) * interval/1000
  rtr <- colSums(cosfactor * get_quantity(x, "ff")[index,] * 3.6 *
                   get_quantity(x, "eta")[index,], na.rm = TRUE) * interval/1000
  vid <- colSums(get_quantity(x, "dens")[index,], na.rm = TRUE) * interval/1000
  vir <- colSums(get_quantity(x, "eta")[index,], na.rm = TRUE) * interval/1000
  height <- colSums((x$heights[index] + x$attributes$where$interval/2) *
                      get_quantity(x, "dens")[index,],
                    na.rm = TRUE)/colSums(get_quantity(x, "dens")[index,],
                                          na.rm = TRUE)
  u <- colSums(get_quantity(x, "u")[index,] * get_quantity(x, "dens")[index,],
               na.rm = TRUE)/colSums(get_quantity(x, "dens")[index,],
                                     na.rm = TRUE)
  v <- colSums(get_quantity(x, "v")[index,] * get_quantity(x, "dens")[index,],
               na.rm = TRUE)/colSums(get_quantity(x, "dens")[index,],
                                     na.rm = TRUE)
  ff <- sqrt(u^2 + v^2)
  dd <- (pi/2 - atan2(v, u)) * 180/pi
  # time-integrated measures:
  dt <- (c(0, x$timesteps) + c(x$timesteps, 0))/2
  dt <- pmin(interval.max, dt)
  # convert to hours
  dt <- as.numeric(dt)/3600
  mt <- cumsum(dt * mtr)
  rt <- cumsum(dt * rtr)
  # prepare output
  output <- data.frame(datetime = x$dates, mtr = mtr, vid = vid, vir = vir,
                       rtr = rtr, mt = mt, rt = rt, ff = ff, dd = dd, u = u,
                       v = v, HGHT = height)
  class(output) <- c("vpi", "data.frame")
  rownames(output) <- NULL
  attributes(output)$alt.min <- alt.min
  attributes(output)$alt.max <- alt.max
  attributes(output)$alpha <- alpha
  attributes(output)$rcs <- rcs(x)
  attributes(output)$lat <- x$attributes$where$lat
  attributes(output)$lon <- x$attributes$where$lon
  return(output)
}
