#' Vertically integrate profiles (`vp` or `vpts`) to an
#' integrated profile (`vpi`)
#'
#' Performs a vertical integration of density, reflectivity and migration
#' traffic rate, and a vertical averaging of ground speed and direction
#' weighted by density.
#'
#' @param x A `vp` or `vpts` object.
#' @param alt_min Minimum altitude in m. `"antenna"` can be used to set the
#' minimum altitude to the height of the antenna.
#' @param alt_max Maximum altitude in m.
#' @param alpha Migratory direction in clockwise degrees from north.
#' @param interval_max Maximum time interval belonging to a single profile in
#' seconds. Traffic rates are set to zero at times `t` for which no
#' profiles can be found within the period `t-interval_max/2` to
#' `t+interval_max/2`. Ignored for single profiles of class `vp`.
#' @param interval_replace Time interval to use for any interval > interval_max.
#' By default the mean of all intervals <= interval_max
#' @param height_quantile For default `NA` the calculated height equals
#' the mean flight altitude. Otherwise a number between 0 and 1 specifying a
#' quantile of the height distribution.
#'
#' @return an object of class `vpi`, a data frame with vertically
#' integrated profile quantities
#'
#' @details
#' \subsection{Available quantities}{
#' The function generates a specially classed data frame with the following
#' quantities:
#'
#' * `datetime`: POSIXct date of each profile in UTC
#' * `vid`: Vertically Integrated Density in individuals/km^2.
#'   `vid` is a surface density, whereas `dens` in `vp` objects is a volume
#'   density.
#' * `vir`: Vertically Integrated Reflectivity in cm^2/km^2
#' * `mtr`: Migration Traffic Rate in individuals/km/h
#' * `rtr`: Reflectivity Traffic Rate in cm^2/km/h
#' * `mt`: Migration Traffic in individuals/km, cumulated from the start of the
#'   time series up to `datetime`
#' * `rt`: Reflectivity Traffic in cm^2/km, cumulated from the start of the time
#'   series up to `datetime`
#' * `ff`: Horizontal ground speed in m/s
#' * `dd`: Direction of the horizontal ground speed in degrees
#' * `u`: Ground speed component west to east in m/s
#' * `v`: Ground speed component south to north in m/s
#' * `height`: Mean flight height (height weighted by eta) in m above sea level
#'
#' Vertically integrated density and reflectivity are related according to
#' \eqn{vid=vir/rcs(x)}, with [rcs] the assumed radar cross section per
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
#' The transect direction is set by the angle `alpha`. When
#' `alpha=NA`, the transect runs perpendicular to the measured migratory
#' direction. `mtr` then equals the number of crossing targets per km
#' transect per hour, for a transect kept perpendicular to the measured
#' migratory movement at all times and altitudes. In this case `mtr` is
#' always a positive quantity, defined as:
#'
#' \deqn{mtr = 3.6 \sum_i \mathit{dens}_i \mathit{ff}_i \Delta h}{mtr = 3.6 \sum_i \mathit{dens}_i \mathit{ff}_i \Delta h}
#'
#' with the sum running over all altitude layers between `alt_min` and
#' `alt_max`, \eqn{\mathit{dens}_i} the bird density, \eqn{\mathit{ff}_i} the ground speed at
#' altitude layer i, and \eqn{\Delta h} the altitude layer width. The factor 3.6
#' refers to a unit conversion of speeds \eqn{\mathit{ff}_i} from m/s to km/h.
#'
#' If `alpha` is given a numeric value, the transect is taken perpendicular
#' to the direction `alpha`, and the number of crossing targets per hour
#' per km transect is calculated as:
#'
#' \deqn{mtr = 3.6 \sum_i \mathit{dens}_i \mathit{ff}_i \cos((dd_i-\alpha) \pi/180) \Delta h}{mtr = 3.6 \sum_i \mathit{dens}_i \mathit{ff}_i \cos((dd_i-\alpha) \pi/180) \Delta h}
#' with \eqn{dd_i} the migratory direction at altitude i.
#'
#' Note that this equation evaluates to the previous equation when `alpha` equals \eqn{dd_i}.
#' Also note we can rewrite this equation using trigonometry as:
#'
#' \deqn{mtr = 3.6 \sum_i \mathit{dens}_i (u_i \sin(\alpha \pi/180) + v_i \cos(\alpha \pi/180)) \Delta h}{mtr = 3.6 \sum_i \mathit{dens}_i (u_i \sin(\alpha \pi/180) + v_i \cos(alpha pi/180)) \Delta h}
#' with \eqn{u_i} and \eqn{v_i} the u and v ground speed components at altitude i.
#'
#' In this definition `mtr` is a traditional flux into a direction of
#' interest. Targets moving into the direction `alpha` contribute
#' positively to `mtr`, while targets moving in the opposite direction
#' contribute negatively to `mtr`. Therefore `mtr` can be both
#' positive or negative, depending on the definition of alpha.
#'
#' Note that `mtr` for a given value of `alpha` can also be calculated from
#' the vertically integrated density `vid` and the height-integrated velocity
#' components `u` and `v` as follows:
#'
#' \deqn{mtr = 3.6 (u \sin(\alpha \pi/180) + v \cos(\alpha \pi/180)) vid}{mtr = 3.6 (u \sin(\alpha \pi/180) + v \cos(\alpha \pi/180)) vid}
#'
#' Formula for reflectivity traffic rate `rtr` are found by replacing
#' `dens` with `eta` and `vid` with `vir` in the formula for `mtr`.
#' Reflectivity traffic rate gives the cross-sectional area
#' passing the radar per km transect perpendicular to the migratory direction per hour.
#' `mtr` values are conditional on settings of [rcs], while `rtr` values are not.
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
#' `mt` values are conditional on settings of [rcs], while `rt` values are not.
#'
#' Columns mt and rt in the output dataframe provides migration traffic as a numeric value equal to
#' migration traffic and reflectivity traffic from the start of the time series up till the moment of the time stamp
#' of the respective row.
#'
#' \subsection{Ground speed (ff) and ground speed components (u,v)}{
#' The height-averaged ground speed is defined as:
#'
#' \deqn{\mathit{ff} = \sum_i \mathit{dens}_i \mathit{ff}_i / \sum_i \mathit{dens}_i}{\mathit{ff} = \sum_i \mathit{dens}_i \mathit{ff}_i / \sum_i \mathit{dens}_i}
#' with the sum running over all altitude layers between `alt_min` and
#' `alt_max`, \eqn{\mathit{dens}_i} the bird density, \eqn{\mathit{ff}_i} the ground speed at
#' altitude layer i.
#'
#' the height-averaged u component (west to east) is defined as:
#'
#' \deqn{u = \sum_i \mathit{dens}_i u_i / \sum_i \mathit{dens}_i}{u = \sum_i \mathit{dens}_i u_i / \sum_i \mathit{dens}_i}
#'
#' the height-averaged v component (south to north) is defined as:
#'
#' \deqn{v = \sum_i \mathit{dens}_i v_i / \sum_i \mathit{dens}_i}{v = \sum_i \mathit{dens}_i v_i / \sum_i \mathit{dens}_i}
#' }
#'
#' Note that \eqn{\mathit{ff}_i=\sqrt(u_i^2 + v_i^2)}, but the same does not hold for the
#' height-integrated speeds, i.e. \eqn{\mathit{ff} \neq \sqrt(u^2 + v^2)} as soon as the
#' ground speed directions vary with altitude.
#'
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
#' # calculate median flight altitude (instead of default mean)
#' integrate_profile(example_vp, height_quantile=.5)
#' # calculate the 90% percentile of the flight altitude distribution
#' integrate_profile(example_vpts, height_quantile=.9)
integrate_profile <- function(x, alt_min, alt_max,
                              alpha = NA, interval_max = 3600,
                              interval_replace = NA, height_quantile = NA) {
  UseMethod("integrate_profile", x)
}

#' @describeIn integrate_profile Vertically integrate a vertical profile.
#'
#' @export
integrate_profile.vp <- function(x, alt_min = 0, alt_max = Inf, alpha = NA,
                                 interval_max = 3600, interval_replace = NA, height_quantile = NA) {
  stopifnot(inherits(x, "vp"))
  stopifnot(is.numeric(alt_min) | alt_min=="antenna")
  stopifnot(is.numeric(alt_max))
  stopifnot(is.na(alpha) || is.numeric(alpha))

  assert_that(is.scalar(height_quantile))
  if(!is.na(height_quantile)){
    assert_that(is.number(height_quantile))
    assert_that(height_quantile>0 && height_quantile<1)
  }

  if (alt_min=="antenna"){
    alt_min = x$attributes$where$height
  }

  if (alt_max <= alt_min) stop("'alt_min' should be smaller than 'alt_max'")

  # Altitudinal resolution of the bin
  interval <- x$attributes$where$interval

  # dh is a vector of the height of each bin used for the computation in km
  # Its value is zeros for bins completly below alt_min or above alt_max, the
  # interval value for all bin completly above alt_min and below alt_max, and
  # a ratio of interval for the bins overlaping alt_min and alt_max
  dh <- pmin(pmin(pmax(x$data$height+x$attributes$where$interval-alt_min,0),interval),
             pmin(pmax(alt_max-x$data$height,0),interval)) / 1000

  # Vertically Integrated Density in individuals/km^2
  vid <- sum(get_quantity(x, "dens") * dh, na.rm = TRUE)
  # Vertically Integrated Reflectivity in cm^2/km^2
  vir <- sum(get_quantity(x, "eta") * dh, na.rm = TRUE)

  # Projection of migratory direction (alpha) on the flight direction (dd)
  if (is.na(alpha)) {
    cosfactor <- rep(1, length(dh))
  } else {
    cosfactor <- cos((get_quantity(x, "dd") - alpha) * pi / 180)
  }

  # Migration Traffic Rate in individuals/km/h
  # multiply speeds (ff) by 3.6 to convert m/s to km/h
  mtr <- sum(get_quantity(x, "dens") * cosfactor * get_quantity(x, "ff")
             * 3.6  * dh, na.rm = TRUE)
  # Reflectivity Traffic Rate in cm^2/km/h
  rtr <- sum(get_quantity(x, "eta") * cosfactor * get_quantity(x, "ff")
             * 3.6 * dh, na.rm = TRUE)

  # The following quantites are vertically summed by weighting each altitudinal
  # bin based on their bird density value (dens) and their height.
  weight_densdh <- get_quantity(x, "dens") * dh
  weight_densdh[is.na(weight_densdh)] <- 0
  weight_densdh <- weight_densdh / sum(weight_densdh)

  if(is.na(height_quantile)){
    # default (no height_quantile specified) is calculating the mean altitude
    height <- weighted.mean(get_quantity(x, "height") + interval / 2, weight_densdh, na.rm = TRUE)
  }
  else{
    # calculate a quantile of the flight altitude distribution
    # 1) integrate over altitude
    denscum=cumsum(weight_densdh)
    denscum[is.na(denscum)]=0
    # 2) find lowerbound index:
    height_index_lower=findInterval(height_quantile, denscum)
    # 3) find the two height bins closest to the quantile of interest
    height_lower=x$data$height[height_index_lower] + interval / 2
    height_upper=x$data$height[min(height_index_lower+1,length(denscum))] + interval / 2
    height_quantile_lower <- denscum[height_index_lower]
    height_quantile_upper <- denscum[min(height_index_lower+1,length(denscum))]
    # 4) do a linear interpolation to estimate the altitude at the quantile of interest
    delta_linear_interpolation <- (height_quantile-height_quantile_lower)*(height_upper-height_lower)/(height_quantile_upper-height_quantile_lower)
    if(is.na(delta_linear_interpolation)) delta_linear_interpolation=0
    # 5) store the quantile flight altitude as height
    height <- height_lower+delta_linear_interpolation
  }

  u <- weighted.mean(get_quantity(x, "u"), weight_densdh, na.rm = TRUE)
  v <- weighted.mean(get_quantity(x, "v"), weight_densdh, na.rm = TRUE)
  ff <- weighted.mean(get_quantity(x, "ff"), weight_densdh, na.rm = TRUE)
  dd <- (pi / 2 - atan2(v, u)) * 180 / pi
  dd[which(dd<0)]=dd[which(dd<0)]+360
  # time-integrated measures not defined for a single profile:
  mt <- NA
  rt <- NA
  # prepare output
  output <- data.frame(
    radar=x$radar, datetime = x$datetime, mtr = mtr, vid = vid, vir = vir,
    rtr = rtr, mt = mt, rt = rt, ff = ff, dd = dd, u = u,
    v = v, height = height
  )

  if ("u_wind" %in% names(x$data) & "v_wind" %in% names(x$data)) {
    airspeed_u <- get_quantity(x, "u") - get_quantity(x, "u_wind")
    airspeed_v <- get_quantity(x, "v") - get_quantity(x, "v_wind")
    output$airspeed <- weighted.mean(sqrt(airspeed_u^2 + airspeed_v^2), weight_densdh, na.rm = TRUE)
    output$heading <- weighted.mean((pi / 2 - atan2(airspeed_v, airspeed_u)) * 180 / pi, weight_densdh, na.rm = TRUE)
    output$heading[which(output$heading<0)]=output$heading[which(output$heading<0)]+360
    output$airspeed_u <- weighted.mean(airspeed_u, weight_densdh, na.rm = TRUE)
    output$airspeed_v <- weighted.mean(airspeed_v, weight_densdh, na.rm = TRUE)
    output$ff_wind <- weighted.mean(sqrt(get_quantity(x,"u_wind")^2 + get_quantity(x,"v_wind")^2), weight_densdh, na.rm = TRUE)
    output$u_wind <- weighted.mean(get_quantity(x,"u_wind"), weight_densdh, na.rm = TRUE)
    output$v_wind <- weighted.mean(get_quantity(x,"v_wind"), weight_densdh, na.rm = TRUE)
  }

  class(output) <- c("vpi", "data.frame")
  rownames(output) <- NULL
  attributes(output)$alt_min <- alt_min
  attributes(output)$alt_max <- alt_max
  attributes(output)$alpha <- alpha
  attributes(output)$interval_max <- interval_max
  attributes(output)$interval_replace <- interval_replace
  attributes(output)$height_quantile <- height_quantile
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
                                   alpha = NA, interval_max = 3600,
                                   interval_replace=NA, height_quantile = NA) {
  vptest <- sapply(x, function(y) is(y, "vp"))
  if (FALSE %in% vptest) {
    stop("requires list of vp objects as input")
  }
  stopifnot(is.numeric(alt_min) | alt_min=="antenna")
  stopifnot(is.numeric(alt_max))

  output <- do.call(rbind, lapply(x, integrate_profile.vp,
    alt_min = alt_min,
    alt_max = alt_max, alpha = alpha,
    interval_max = interval_max, interval_replace=interval_replace
  ))
  class(output) <- c("vpi", "data.frame")
  attributes(output)$radar <- x$radar
  attributes(output)$alt_min <- alt_min
  attributes(output)$alt_max <- alt_max
  attributes(output)$alpha <- alpha
  attributes(output)$interval_max <- interval_max
  attributes(output)$interval_replace <- interval_replace
  attributes(output)$height_quantile <- height_quantile
  attributes(output)$rcs <- rcs(x)
  # TODO set lat/lon attributes
  return(output)
}

#' @describeIn integrate_profile Vertically integrate a time series of
#' vertical profiles.
#'
#' @export
integrate_profile.vpts <- function(x, alt_min = 0, alt_max = Inf,
                                   alpha = NA, interval_max = 3600,
                                   interval_replace = NA, height_quantile = NA) {
  stopifnot(inherits(x, "vpts"))
  stopifnot(is.numeric(alt_min) | alt_min=="antenna")
  stopifnot(is.numeric(alt_max))
  stopifnot(is.na(alpha) || is.numeric(alpha))
  assert_that(is.number(interval_max))
  assert_that(interval_max>0)

  dt_median <- as.double(median(x$timesteps),unit="secs")
  if(interval_max < dt_median) warning(paste0("interval_max < median timestep of the time series (",dt_median," sec), consider a larger value."))

  if(!missing(interval_replace)){
    assert_that(is.number(interval_replace))
    assert_that(interval_replace>0)
  }
  else{
    interval_replace=as.double(mean(x$timesteps[x$timesteps<interval_max]),unit="secs")
    if(is.na(interval_replace)) interval_replace=interval_max
  }

  assert_that(is.scalar(height_quantile))
  if(!is.na(height_quantile)){
    assert_that(is.number(height_quantile))
    assert_that(height_quantile>0 && height_quantile<1)
  }

  # Integrate from antenna height
  if (alt_min=="antenna"){
    alt_min = x$attributes$where$height
  }

  if (alt_max <= alt_min) stop("'alt_min' should be smaller than 'alt_max'")

  # Altitudinal resolution of the bin
  interval <- x$attributes$where$interval

  # dh is a vector of the height of each bin used for the computation in km
  # Its value is zeros for bins completly below alt_min or above alt_max, the
  # interval value for all bin completly above alt_min and below alt_max, and
  # a ratio of interval for the bins overlaping alt_min and alt_max
  dh <- pmin(pmin(pmax(x$height+x$attributes$where$interval-alt_min,0),interval),
             pmin(pmax(alt_max-x$height,0),interval)) / 1000

  # Vertically Integrated Density in individuals/km^2
  vid <- colSums(get_quantity(x, "dens") * dh, na.rm = TRUE)
  # Vertically Integrated Reflectivity in cm^2/km^2
  vir <- colSums(get_quantity(x, "eta") * dh, na.rm = TRUE)

  # Projection of migratory direction (alpha) on the flight direction (dd)
  if (is.na(alpha)) {
    cosfactor <- 1 + 0 * get_quantity(x, "dd")
  } else {
    cosfactor <- cos((get_quantity(x, "dd") - alpha) * pi / 180)
  }

  # Migration Traffic Rate in individuals/km/h
  # multiply speeds (ff) by 3.6 to convert m/s to km/h
  mtr <- colSums(get_quantity(x, "dens") * cosfactor * get_quantity(x, "ff")
             * 3.6  * dh, na.rm = TRUE)
  # Reflectivity Traffic Rate in cm^2/km/h
  rtr <- colSums(get_quantity(x, "eta") * cosfactor * get_quantity(x, "ff")
             * 3.6 * dh, na.rm = TRUE)

  # The following quantites are vertically summed by weighting each altitudinal
  # bin based on their bird density value (dens) and their height.
  weight_densdh <- get_quantity(x, "dens") * dh
  weight_densdh[is.na(weight_densdh)] <- 0
  # Normalize the weight of each vp by its column sum.
  weight_densdh <- sweep(weight_densdh, 2, colSums(weight_densdh), FUN="/")
  # Find index where no bird are present
  no_bird <- is.na(colSums(weight_densdh))

  if(is.na(height_quantile)){
    # default (no height_quantile specified) is calculating the mean altitude
    height <- colSums( (get_quantity(x, "height") + interval / 2) * weight_densdh, na.rm = T)
  }
  else{
    # calculate a quantile of the flight altitude distribution
    # 1) integrate over altitude
    denscum=apply(weight_densdh, 2, cumsum)
    denscum[is.na(denscum)]=0
    # 2) find lowerbound index:
    height_index_lower=apply(denscum,2,findInterval,x=height_quantile)
    # 3) find the two height bins closest to the quantile of interest
    height_lower=x$height[height_index_lower] + interval / 2
    height_upper=x$height[pmin(height_index_lower+1,nrow(denscum))] + interval / 2
    height_quantile_lower <- denscum[seq(0,nrow(denscum)*(ncol(denscum)-1),nrow(denscum))+height_index_lower]
    height_quantile_upper <- denscum[seq(0,nrow(denscum)*(ncol(denscum)-1),nrow(denscum))+pmin(height_index_lower+1,nrow(denscum))]
    # 4) do a linear interpolation to estimate the altitude at the quantile of interest
    delta_linear_interpolation <- (height_quantile-height_quantile_lower)*(height_upper-height_lower)/(height_quantile_upper-height_quantile_lower)
    delta_linear_interpolation[is.na(delta_linear_interpolation)]=0
    # 5) store the quantile flight altitude as height
    height <- height_lower+delta_linear_interpolation
  }

  height[no_bird] <- NA
  u <- colSums( get_quantity(x, "u") * weight_densdh, na.rm = T)
  u[no_bird] <- NA
  v <- colSums( get_quantity(x, "v") * weight_densdh, na.rm = T)
  v[no_bird] <- NA
  ff <- colSums( get_quantity(x, "ff") * weight_densdh, na.rm = T)
  ff[no_bird] <- NA
  dd <- (pi / 2 - atan2(v, u)) * 180 / pi
  dd[which(dd<0)]=dd[which(dd<0)]+360
  dd[no_bird] <- NA

  # time-integrated measures:
  dt <- (c(0, x$timesteps) + c(x$timesteps, 0)) / 2
  dt <- pmin(interval_replace, dt)
  # convert to hours
  dt <- as.numeric(dt) / 3600
  mt <- cumsum(dt * mtr)
  rt <- cumsum(dt * rtr)
  # prepare output
  output <- data.frame(
    radar = x$radar, datetime = x$datetime, mtr = mtr, vid = vid, vir = vir,
    rtr = rtr, mt = mt, rt = rt, ff = ff, dd = dd, u = u,
    v = v, height = height
  )

  if ("u_wind" %in% names(x$data) & "v_wind" %in% names(x$data)) {
    airspeed_u <- get_quantity(x, "u") - get_quantity(x, "u_wind")
    airspeed_v <- get_quantity(x, "v") - get_quantity(x, "v_wind")
    output$airspeed <- colSums(sqrt(airspeed_u^2 + airspeed_v^2) * weight_densdh, na.rm = TRUE)
    output$heading <- colSums(((pi / 2 - atan2(airspeed_v, airspeed_u)) * 180 / pi) * weight_densdh, na.rm = TRUE)
    output$heading[which(output$heading<0)]=output$heading[which(output$heading<0)]+360
    output$airspeed_u <- colSums(airspeed_u * weight_densdh, na.rm = TRUE)
    output$airspeed_v <- colSums(airspeed_v * weight_densdh, na.rm = TRUE)
    output$ff_wind <- colSums(sqrt(get_quantity(x,"u_wind")^2 + get_quantity(x,"v_wind")^2) * weight_densdh, na.rm = TRUE)
    output$u_wind <- colSums(get_quantity(x,"u_wind") * weight_densdh, na.rm = TRUE)
    output$v_wind <- colSums(get_quantity(x,"v_wind") * weight_densdh, na.rm = TRUE)
  }

  class(output) <- c("vpi", "data.frame")
  rownames(output) <- NULL
  attributes(output)$radar <- x$radar
  attributes(output)$alt_min <- alt_min
  attributes(output)$alt_max <- alt_max
  attributes(output)$alpha <- alpha
  attributes(output)$interval_max <- interval_max
  attributes(output)$interval_replace <- interval_replace
  attributes(output)$height_quantile <- height_quantile
  attributes(output)$rcs <- rcs(x)
  attributes(output)$lat <- x$attributes$where$lat
  attributes(output)$lon <- x$attributes$where$lon
  return(output)
}
