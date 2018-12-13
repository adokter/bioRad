#' Vertically integrate profiles (\code{vp} or \code{vpts}) to an
#' integrated profile (\code{vpi})
#'
#' Performs a vertical integration of density, reflectivity and migration
#' traffic rate, and a vertical averaging of ground speed and direction
#' weighted by density.
#'
#' @param x A \code{vp} or \code{vpts} object.
#' @param alt_min Minimum altitude in m.
#' @param alt_max Maximum altitude in m.
#' @param alpha Migratory direction in clockwise degrees from north.
#' @param interval_max Maximum time interval belonging to a single profile in
#' seconds. Traffic rates are set to zero at times \code{t} for which no
#' profiles can be found within the period \code{t-interval_max/2} to
#' \code{t+interval_max/2}. Ignored for single profiles of class \code{vp}.
#'
#' @return an object of class \code{vpi}, a data frame with vertically
#' integrated profile quantities
#'
#' @details
#' \subsection{Available quantities}{
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
#'    \item{\code{mt}}{Migration Traffic in individuals/km}
#'    \item{\code{rt}}{Reflectivity Traffic in cm^2/km}
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
#' }
#'
#' \subsection{Migration traffic rate (mtr) and reflectivity traffic rate (rtr)}{
#' Migration traffic rate (mtr) for an altitude layer is a flux measure, defined
#' as the number of targets crossing a unit of transect per hour.
#'
#' Column mtr of the output dataframe gives migration traffic rates in individuals/km/hour.
#'
#' The transect direction is set by the angle \code{alpha}. When
#' \code{alpha=NA}, the transect runs perpendicular to the measured migratory
#' direction. \code{mtr} then equals the number of crossing targets per km
#' transect per hour, for a transect kept perpendicular to the measured
#' migratory movement at all times and altitudes. In this case \code{mtr} is
#' always a positive quantity, defined as:
#'
#' \deqn{mtr = \sum_i dens_i ff_i \Delta h}{mtr = \sum_i dens_i ff_i \Delta h}
#'
#' with the sum running over all altitude layers between \code{alt_min} and
#' \code{alt_max}, \eqn{dens_i} the bird density, \eqn{ff_i} the ground speed at
#' altitude layer i, and \eqn{\Delta h} the altitude layer width.
#'
#' If \code{alpha} is given a numeric value, the transect is taken perpendicular
#' to the direction \code{alpha}, and the number of crossing targets per hour
#' per km transect is calculated as:
#'
#' \deqn{mtr = \sum_i dens_i ff_i \cos(dd_i-alpha) \Delta h}{mtr = \sum_i dens_i ff_i \cos(dd_i-alpha) \Delta h}
#' with \eqn{dd_i} the migratory direction at altitude i.
#'
#' Note that this equation evaluates to the previous equation when \code{alpha} equals \eqn{dd_i}.
#'
#' In this definition \code{mtr} is a traditional flux into a direction of
#' interest. Targets moving into the direction \code{alpha} contribute
#' positively to \code{mtr}, while targets moving in the opposite direction
#' contribute negatively to \code{mtr}. Therefore \code{mtr} can be both
#' positive or negative, depending on the definition of alpha.
#'
#' Formula for reflectivity traffic rate \code{rtr} are found by replacing
#' \code{dens} with \code{eta} in the formula for \code{mtr}.
#' Reflectivity traffic rate gives the cross-sectional area
#' passing the radar per km transect perpendicular to the migratory direction per hour.
#' \code{rtr} values are conditional on settings of \link{rcs}, while \code{mtr} values are not.
#' }
#'
#' \subsection{Migration traffic (mt) and reflectivity traffic (rt)}{
#' Migration traffic is calculated by time-integration of
#' migration traffic rates. Migration traffic gives the number of individuals
#' that have passed per km perpendicular to the migratory direction at the
#' position of the radar for the full period of the time series within the
#' specified altitude band.
#'
#' Reflectivity traffic is calculated by time-integration of
#' refletivity traffic rates. Reflectivity traffic gives the total cross-sectional area
#' that has passed per km perpendicular to the migratory direction at the
#' position of the radar for the full period of the time series within the
#' specified altitude band.
#'
#' \code{rt} values are conditional on settings of \link{rcs}, while \code{mt} values are not.
#'
#' Columnns mt and rt in the output dataframe provides migration traffic as a numeric value equal to
#' migration traffic and reflectivity traffic from the start of the time series up till the moment of the time stamp
#' of the respective row.
#' }
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
#' plot(integrate_profile(example_vpts, alt_min = 1000))
#' # plot the (cumulative) migration traffic
#' plot(integrate_profile(example_vpts), quantity = "mt")
integrate_profile <- function(x, alt_min, alt_max,
                              alpha = NA, interval_max = Inf) {
  UseMethod("integrate_profile", x)
}

#' @describeIn integrate_profile Vertically integrate a vertical profile.
#'
#' @export
integrate_profile.vp <- function(x, alt_min = 0, alt_max = Inf, alpha = NA,
                                 interval_max = Inf) {
  stopifnot(inherits(x, "vp"))
  stopifnot(is.numeric(alt_min) & is.numeric(alt_max))
  stopifnot(is.na(alpha) || is.numeric(alpha))

  interval <- x$attributes$where$interval

  if(alt_max <= alt_min) stop("'alt_min' should be smaller than 'alt_max'")

  alt_min=max(alt_min,min(x$data$HGHT))
  alt_max=min(alt_max,max(x$data$HGHT)+interval)
  if(alt_max-alt_min <= interval) stop(paste("selected altitude range (" ,alt_min,"-",alt_max, " m) should be wider than the width of a single altitude layer (",interval," m)",sep=""))

  index <- which(x$data$HGHT >= alt_min & x$data$HGHT < alt_max)
  if (is.na(alpha)) {
    cosfactor <- rep(1, length(index))
  } else {
    cosfactor <- cos((get_quantity(x, "dd")[index] - alpha) * pi / 180)
  }
  # multiply speeds by 3.6 to convert m/s to km/h
  mtr <- sum(get_quantity(x, "dens")[index] * cosfactor *
    get_quantity(x, "ff")[index] * 3.6 * interval / 1000, na.rm = TRUE)
  rtr <- sum(get_quantity(x, "eta")[index] * cosfactor *
    get_quantity(x, "ff")[index] * 3.6 * interval / 1000, na.rm = TRUE)
  vid <- sum(get_quantity(x, "dens")[index], na.rm = TRUE) * interval / 1000
  vir <- sum(get_quantity(x, "eta")[index], na.rm = TRUE) * interval / 1000
  height <- sum((get_quantity(x, "HGHT") + x$attributes$where$interval / 2) *
    get_quantity(x, "dens")[index], na.rm = TRUE) / sum(
    get_quantity(x, "dens")[index],
    na.rm = TRUE
  )
  u <- sum(get_quantity(x, "u")[index] * get_quantity(x, "dens")[index],
    na.rm = TRUE
  ) / sum(get_quantity(x, "dens")[index], na.rm = TRUE)
  v <- sum(get_quantity(x, "v")[index] * get_quantity(x, "dens")[index],
    na.rm = TRUE
  ) / sum(get_quantity(x, "dens")[index], na.rm = TRUE)
  ff <- sqrt(u^2 + v^2)
  dd <- (pi / 2 - atan2(v, u)) * 180 / pi
  # time-integrated measures not defined for a single profile:
  mt <- NA
  rt <- NA
  # prepare output
  output <- data.frame(
    datetime = x$datetime, mtr = mtr, vid = vid, vir = vir,
    rtr = rtr, mt = mt, rt = rt, ff = ff, dd = dd, u = u,
    v = v, HGHT = height
  )
  class(output) <- c("vpi", "data.frame")
  rownames(output) <- NULL
  attributes(output)$alt_min <- alt_min
  attributes(output)$alt_max <- alt_max
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
integrate_profile.list <- function(x, alt_min = 0, alt_max = Inf,
                                   alpha = NA, interval_max = Inf) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires list of vp objects as input")
  }
  stopifnot(is.numeric(alt_min) & is.numeric(alt_max))

  output <- do.call(rbind, lapply(x, integrate_profile.vp,
    alt_min = alt_min,
    alt_max = alt_max, alpha = alpha,
    interval_max = interval_max
  ))
  class(output) <- c("vpi", "data.frame")
  attributes(output)$alt_min <- alt_min
  attributes(output)$alt_max <- alt_max
  attributes(output)$alpha <- alpha
  attributes(output)$rcs <- rcs(x)
  # TODO set lat/lon attributes
  return(output)
}

#' @describeIn integrate_profile Vertically integrate a time series of
#' vertical profiles.
#'
#' @export
integrate_profile.vpts <- function(x, alt_min = 0, alt_max = Inf,
                                   alpha = NA, interval_max = Inf) {
  stopifnot(inherits(x, "vpts"))
  stopifnot(is.numeric(alt_min) & is.numeric(alt_max))
  stopifnot(is.na(alpha) || is.numeric(alpha))

  interval <- x$attributes$where$interval

  if(alt_max <= alt_min) stop("'alt_min' should be smaller than 'alt_max'")

  alt_min=max(alt_min,min(x$heights))
  alt_max=min(alt_max,max(x$heights)+interval)
  if(alt_max-alt_min <= interval) stop(paste("selected altitude range (" ,alt_min,"-",alt_max, " m) should be wider than the width of a single altitude layer (",interval," m)",sep=""))

  index <- which(x$heights >= alt_min & x$heights < alt_max)
  if (is.na(alpha)) {
    cosfactor <- 1 + 0 * get_quantity(x, "dd")[index, ]
  } else {
    cosfactor <- cos((get_quantity(x, "dd")[index, ] - alpha) * pi / 180)
  }
  # multiply speeds by 3.6 to convert m/s to km/h
  mtr <- colSums(cosfactor * get_quantity(x, "ff")[index, ] * 3.6 *
    get_quantity(x, "dens")[index, ], na.rm = TRUE) * interval / 1000
  rtr <- colSums(cosfactor * get_quantity(x, "ff")[index, ] * 3.6 *
    get_quantity(x, "eta")[index, ], na.rm = TRUE) * interval / 1000
  vid <- colSums(get_quantity(x, "dens")[index, ], na.rm = TRUE) * interval / 1000
  vir <- colSums(get_quantity(x, "eta")[index, ], na.rm = TRUE) * interval / 1000
  height <- colSums((x$heights[index] + x$attributes$where$interval / 2) *
    get_quantity(x, "dens")[index, ],
  na.rm = TRUE
  ) / colSums(get_quantity(x, "dens")[index, ],
    na.rm = TRUE
  )
  u <- colSums(get_quantity(x, "u")[index, ] * get_quantity(x, "dens")[index, ],
    na.rm = TRUE
  ) / colSums(get_quantity(x, "dens")[index, ],
    na.rm = TRUE
  )
  v <- colSums(get_quantity(x, "v")[index, ] * get_quantity(x, "dens")[index, ],
    na.rm = TRUE
  ) / colSums(get_quantity(x, "dens")[index, ],
    na.rm = TRUE
  )
  ff <- sqrt(u^2 + v^2)
  dd <- (pi / 2 - atan2(v, u)) * 180 / pi
  # time-integrated measures:
  dt <- (c(0, x$timesteps) + c(x$timesteps, 0)) / 2
  dt <- pmin(interval_max, dt)
  # convert to hours
  dt <- as.numeric(dt) / 3600
  mt <- cumsum(dt * mtr)
  rt <- cumsum(dt * rtr)
  # prepare output
  output <- data.frame(
    datetime = x$datetime, mtr = mtr, vid = vid, vir = vir,
    rtr = rtr, mt = mt, rt = rt, ff = ff, dd = dd, u = u,
    v = v, HGHT = height
  )
  class(output) <- c("vpi", "data.frame")
  rownames(output) <- NULL
  attributes(output)$alt_min <- alt_min
  attributes(output)$alt_max <- alt_max
  attributes(output)$alpha <- alpha
  attributes(output)$rcs <- rcs(x)
  attributes(output)$lat <- x$attributes$where$lat
  attributes(output)$lon <- x$attributes$where$lon
  return(output)
}
