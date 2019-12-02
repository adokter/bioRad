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
#'    \item{\code{mt}}{Migration Traffic in individuals/km, cumulated from
#'       the start of the time series up to \code{datetime}}
#'    \item{\code{rt}}{Reflectivity Traffic in cm^2/km, cumulated from
#'       the start of the time series up to \code{datetime}}
#'    \item{\code{ff}}{Horizontal ground speed in m/s}
#'    \item{\code{dd}}{Horizontal ground speed direction in degrees}
#'    \item{\code{u}}{Ground speed component west to east in m/s}
#'    \item{\code{v}}{Ground speed component north to south in m/s}
#'    \item{\code{height}}{Mean flight height (height weighted by eta) in m above sea level}
#' }
#' Vertically integrated density and reflectivity are related according to
#' \eqn{vid=vir/rcs(x)}, with \link{rcs} the assumed radar cross section per
#' individual. Similarly, migration traffic rate and reflectivity traffic rate
#' are related according to \eqn{mtr=rtr/rcs(x)}
#' }
#'
#' \subsection{Ground speed (ff) and ground speed components (u,v)}{
#' The height-averaged ground speed is defined as:
#'
#' \deqn{ff = \sum_i dens_i ff_i / \sum_i dens_i}{ff = \sum_i dens_i ff_i / \sum_i dens_i}
#' with the sum running over all altitude layers between \code{alt_min} and
#' \code{alt_max}, \eqn{dens_i} the bird density, \eqn{ff_i} the ground speed at
#' altitude layer i.
#'
#' the height-averaged u component (west to east) is defined as:
#'
#' \deqn{u = \sum_i dens_i u_i / \sum_i dens_i}{u = \sum_i dens_i u_i / \sum_i dens_i}
#'
#' the height-averaged v component (south to north) is defined as:
#'
#' \deqn{v = \sum_i dens_i v_i / \sum_i dens_i}{v = \sum_i dens_i v_i / \sum_i dens_i}
#' }
#'
#' Note that \eqn{ff_i=\sqrt(u_i^2 + v_i^2)}, but the same does not hold for the
#' height-integrated speeds, i.e. \eqn{ff != \sqrt(u^2 + v^2)} as soon as the
#' ground speed directions vary with altitude.
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
#' \deqn{mtr = 3.6 \sum_i dens_i ff_i \Delta h}{mtr = 3.6 \sum_i dens_i ff_i \Delta h}
#'
#' with the sum running over all altitude layers between \code{alt_min} and
#' \code{alt_max}, \eqn{dens_i} the bird density, \eqn{ff_i} the ground speed at
#' altitude layer i, and \eqn{\Delta h} the altitude layer width. The factor 3.6
#' refers to a unit conversion of speeds \eqn{ff_i} from m/s to km/h.
#'
#' If \code{alpha} is given a numeric value, the transect is taken perpendicular
#' to the direction \code{alpha}, and the number of crossing targets per hour
#' per km transect is calculated as:
#'
#' \deqn{mtr = 3.6 \sum_i dens_i ff_i \cos((dd_i-alpha) pi/180) \Delta h}{mtr = 3.6 \sum_i dens_i ff_i \cos((dd_i-alpha) pi/180) \Delta h}
#' with \eqn{dd_i} the migratory direction at altitude i.
#'
#' Note that this equation evaluates to the previous equation when \code{alpha} equals \eqn{dd_i}.
#' Also note we can rewrite this equation using trigonemetry as:
#'
#' \deqn{mtr = 3.6 \sum_i dens_i (u_i \sin(alpha pi/180) + v_i \cos(alpha pi/180)) \Delta h}{mtr = 3.6 \sum_i dens_i (u_i \sin(alpha pi/180) + v_i \cos(alpha pi/180)) \Delta h}
#' with \eqn{u_i} and \eqn{v_i} the u and v ground speed components at altitude i.
#'
#' In this definition \code{mtr} is a traditional flux into a direction of
#' interest. Targets moving into the direction \code{alpha} contribute
#' positively to \code{mtr}, while targets moving in the opposite direction
#' contribute negatively to \code{mtr}. Therefore \code{mtr} can be both
#' positive or negative, depending on the definition of alpha.
#'
#' Note that \code{mtr} for a given value of \code{alpha} can also be calculated from
#' the vertically integrated density \code{vid} and the height-integrated velocity
#' components \code{u} and \code{v} as follows:
#'
#' \deqn{mtr = 3.6 (u \sin(alpha pi/180) + v \cos(alpha pi/180)) vid}{mtr = 3.6 (u \sin(alpha pi/180) + v \cos(alpha pi/180)) vid}
#'
#' Formula for reflectivity traffic rate \code{rtr} are found by replacing
#' \code{dens} with \code{eta} and \code{vid} with \code{vir} in the formula for \code{mtr}.
#' Reflectivity traffic rate gives the cross-sectional area
#' passing the radar per km transect perpendicular to the migratory direction per hour.
#' \code{mtr} values are conditional on settings of \link{rcs}, while \code{rtr} values are not.
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
#' reflectivity traffic rates. Reflectivity traffic gives the total cross-sectional area
#' that has passed per km perpendicular to the migratory direction at the
#' position of the radar for the full period of the time series within the
#' specified altitude band.
#'
#' \code{mt} values are conditional on settings of \link{rcs}, while \code{rt} values are not.
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

  if (alt_max <= alt_min) stop("'alt_min' should be smaller than 'alt_max'")

  if('height' %in% names(x$data))
  {
    height<-x$data$height
  }else{
    height<-x$data$HGHT
  }
  alt_min <- max(alt_min, min(height))
  alt_max <- min(alt_max, max(height) + interval)
  if (alt_max - alt_min <= interval) stop(paste("selected altitude range (", alt_min, "-", alt_max, " m) should be wider than the width of a single altitude layer (", interval, " m)", sep = ""))

  index <- which(height >= alt_min & height < alt_max)
  if (is.na(alpha)) {
    cosfactor <- rep(1, length(index))
  } else {
    cosfactor <- cos((get_quantity(x, "dd")[index] - alpha) * pi / 180)
  }
  dens_quantity <- get_quantity(x, "dens")[index]
  dens_quantity[is.na(dens_quantity)] <- 0

  # multiply speeds by 3.6 to convert m/s to km/h
  mtr <- sum(dens_quantity * cosfactor *
    get_quantity(x, "ff")[index] * 3.6 * interval / 1000, na.rm = TRUE)
  rtr <- sum(get_quantity(x, "eta")[index] * cosfactor *
    get_quantity(x, "ff")[index] * 3.6 * interval / 1000, na.rm = TRUE)
  vid <- sum(dens_quantity, na.rm = TRUE) * interval / 1000
  vir <- sum(get_quantity(x, "eta")[index], na.rm = TRUE) * interval / 1000
  height <- weighted.mean(get_quantity(x, ifelse('height'%in% names(x$data),"height",'HGHT'))[index] + x$attributes$where$interval / 2, dens_quantity, na.rm = TRUE)

  u <- weighted.mean(get_quantity(x, "u")[index], dens_quantity, na.rm = TRUE)
  v <- weighted.mean(get_quantity(x, "v")[index], dens_quantity, na.rm = TRUE)
  ff <- weighted.mean(get_quantity(x, "ff")[index], dens_quantity, na.rm = TRUE)
  dd <- (pi / 2 - atan2(v, u)) * 180 / pi
  # time-integrated measures not defined for a single profile:
  mt <- NA
  rt <- NA
  # prepare output
  output <- data.frame(
    datetime = x$datetime, mtr = mtr, vid = vid, vir = vir,
    rtr = rtr, mt = mt, rt = rt, ff = ff, dd = dd, u = u,
    v = v, height = height
  )

  if ("u_wind" %in% names(x$data) & "v_wind" %in% names(x$data)) {
    airspeed_u <- get_quantity(x, "u")[index] - get_quantity(x, "u_wind")[index]
    airspeed_v <- get_quantity(x, "v")[index] - get_quantity(x, "v_wind")[index]
    output$airspeed <- weighted.mean(sqrt(airspeed_u^2 + airspeed_v^2), dens_quantity, na.rm = TRUE)
    output$heading <- weighted.mean((pi / 2 - atan2(airspeed_v, airspeed_u)) * 180 / pi, dens_quantity, na.rm = TRUE)
    output$airspeed_u <- weighted.mean(airspeed_u, dens_quantity, na.rm = TRUE)
    output$airspeed_v <- weighted.mean(airspeed_u, dens_quantity, na.rm = TRUE)
  }

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

  if (alt_max <= alt_min) stop("'alt_min' should be smaller than 'alt_max'")

  alt_min <- max(alt_min, min(x$height))
  alt_max <- min(alt_max, max(x$height) + interval)
  if (alt_max - alt_min <= interval) stop(paste("selected altitude range (", alt_min, "-", alt_max, " m) should be wider than the width of a single altitude layer (", interval, " m)", sep = ""))

  index <- which(x$height >= alt_min & x$height < alt_max)
  if (is.na(alpha)) {
    cosfactor <- 1 + 0 * get_quantity(x, "dd")[index, ]
  } else {
    cosfactor <- cos((get_quantity(x, "dd")[index, ] - alpha) * pi / 180)
  }

  dens_quantity <- colSums(get_quantity(x, "dens")[index, ], na.rm = TRUE)

  # multiply speeds by 3.6 to convert m/s to km/h
  mtr <- colSums(cosfactor * get_quantity(x, "ff")[index, ] * 3.6 *
    get_quantity(x, "dens")[index, ], na.rm = TRUE) * interval / 1000
  rtr <- colSums(cosfactor * get_quantity(x, "ff")[index, ] * 3.6 *
    get_quantity(x, "eta")[index, ], na.rm = TRUE) * interval / 1000
  vid <- dens_quantity * interval / 1000
  vir <- colSums(get_quantity(x, "eta")[index, ], na.rm = TRUE) * interval / 1000
  height <- colSums((x$height[index] + x$attributes$where$interval / 2) *
    get_quantity(x, "dens")[index, ],
  na.rm = TRUE
  ) / dens_quantity
  u <- colSums(get_quantity(x, "u")[index, ] * get_quantity(x, "dens")[index, ],
    na.rm = TRUE
  ) / dens_quantity
  v <- colSums(get_quantity(x, "v")[index, ] * get_quantity(x, "dens")[index, ],
    na.rm = TRUE
  ) / dens_quantity
  ff <- colSums(get_quantity(x, "ff")[index, ] * get_quantity(x, "dens")[index, ],
    na.rm = TRUE
  ) / dens_quantity
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
    v = v, height = height
  )

  if ("u_wind" %in% names(x$data) & "v_wind" %in% names(x$data)) {
    airspeed_u <- get_quantity(x, "u")[index, ] - get_quantity(x, "u_wind")[index, ]
    airspeed_v <- get_quantity(x, "v")[index, ] - get_quantity(x, "v_wind")[index, ]
    output$airspeed <- colSums(sqrt(airspeed_u^2 + airspeed_v^2) * get_quantity(x, "dens")[index, ], na.rm = TRUE) /
      dens_quantity
    output$heading <- colSums(((pi / 2 - atan2(airspeed_v, airspeed_u)) * 180 / pi) * get_quantity(x, "dens")[index, ], na.rm = TRUE) / dens_quantity
    output$airspeed_u <- colSums(airspeed_u * get_quantity(x, "dens")[index, ], na.rm = TRUE) / dens_quantity
    output$airspeed_v <- colSums(airspeed_v * get_quantity(x, "dens")[index, ], na.rm = TRUE) / dens_quantity
  }

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
