#' Migration traffic rate
#'
#' Migration traffic rate (MTR) for an altitude layer, defined as the
#' number of targets crossing a 1 km line perpendicular to the migratory movement per hour
#' @param x A \code{vp}, list of \code{vp} objects or \code{vpts} object.
#' @param alt.min minimum altitude in m
#' @param alt.max maximum altitude in m
#' @param alpha (optional) migratory direction of interest in clockwise degrees from north, otherwise \code{NA}
#' @export
#' @return an atomic vector of migration traffic rates in individuals/km/hour
#' @details
#' Migration traffic rate (MTR) for an altitude layer is a flux measure, defined as the
#' number of targets crossing a unit of transect per hour.
#'
#' The transect direction is set by the angle \code{alpha}. When \code{alpha=NA},
#' the transect runs perpendicular to the measured migratory direction. \code{mtr} then equals the
#' number of crossing targets per km transect per hour, for a transect kept perpendicular to the
#' measured migratory movement at all times and altitudes. In this case \code{mtr} is always a positive quantity,
#' defined as:
#' \deqn{mtr = \sum_i dens_i ff_i \Delta h}{mtr = \sum_i dens_i ff_i \Delta h}
#' with the sum running over all altitude layers between \code{alt.min} and \code{alt.max}, \eqn{dens_i} the bird density,
#' \eqn{ff_i} the ground speed at altitude layer i, and \eqn{\Delta h} the altitude layer width.
#'
#' If \code{alpha} is given a numeric value, the transect is taken perpendicular to
#' the direction \code{alpha}, and the number of crossing targets per hour per
#' km transect is calculated as:
#'
#' \deqn{mtr = \sum_i dens_i ff_i \cos(dd_i-alpha) \Delta h}{mtr = \sum_i dens_i ff_i \cos(dd_i-alpha) \Delta h}
#' with \eqn{dd_i} the migratory direction at altitude i.
#'
#' Note that this equation evaluates to the previous equation when \code{alpha} equals \eqn{dd_i}.
#' In this definition \code{mtr} is a traditional flux into a direction of interest.
#' Targets moving into the direction \code{alpha} contribute positively to \code{mtr}, while targets moving in the
#' opposite direction contribute negatively to \code{mtr}. Therefore \code{mtr} can be both positive or negative,
#' depending on the definition of alpha.
#'
#' This is a wrapper function for \link{integrate_profile}, extracting only the
#' migration traffic rate data.
#'
#' @examples
#' ### MTR for a single vertical profile ###
#' mtr(example_vp)
#' ### MTRs for a time series of vertical profiles ###
#' # load example time series
#' data(example_vpts)
#' # print migration traffic rates:
#' mtr(example_vpts)
#' # to plot migration traffic rate data, use integrate_profile:
#' plot(integrate_profile(example_vpts), quantity = "mtr")
mtr <- function(x, alt.min=0, alt.max=Inf, alpha=NA) {
  stopifnot(inherits(x, "vp") || inherits(x, "vpts") || inherits(x, "list"))
  if (inherits(x, "list")){
    vptest <- sapply(x, function(y) is(y, "vp"))
    if (FALSE %in% vptest) {
      stop("Not all objects in list are vp objects")
    }
  }
  .Deprecated("integrate_profile")
  .Deprecated(msg="'mtr' has been moved to the 'mtr' column in the output of integrate_profile()")
  vintegrated=integrate_profile(x,alt.min=alt.min,alt.max=alt.max,alpha=alpha)
  vintegrated$mtr
}

#' Migration traffic
#'
#' Total migration traffic, which is calculated by time-integration
#' of migration traffic rates. Migration traffic gives the number of individuals
#' that have passed per km perpendicular to the migratory direction at the
#' position of the radar for the full period of the time series
#' within the specified altitude band.
#' @param x an object inhereting from class '\code{vpts}'
#' @param interval.max maximum time interval belonging to a single profile in seconds. Traffic rates are
#' set to zero at times \code{t} for which no profiles can be found within the period \code{t-interval.max/2} to \code{t+interval.max/2}.
#' @inheritParams mtr
#' @export
#' @return a numeric value equal to migration traffic in number of individuals / km
#' @examples
#' # get example time series object:
#' data(example_vpts)
#' example_vpts
#' # total migration traffic in full altitude band
#' mt(example_vpts)
#' # total migration traffic in 0-1000 meter band
#' mt(example_vpts, alt.min = 0, alt.max = 1000)
mt <- function(x,alt.min=0, alt.max=Inf, alpha=NA,interval.max=Inf){
  stopifnot(inherits(x,"vpts"))
  .Deprecated("integrate_profile")
  .Deprecated(msg="'mt' has been moved to the 'mt' column in the output of integrate_profile()")
  cmt(x,alt.min,alt.max,alpha,interval.max)[ncol(x)]
}

#' Cumulative migration traffic
#'
#' Cumulative migration traffic is calculated as the cumulative sum
#' of the migration traffic within each time step of a time series.
#' Cumulative migration traffic gives the number of individuals
#' that have passed per km perpendicular to the migratory direction at the
#' position of the radar as a function oftime from the start of time series
#' within the specified altitude band.
#' @param x an object inhereting from class '\code{vpts}'
#' @param interval.max maximum time interval belonging to a single profile in seconds. Traffic rates are
#' set to zero at times \code{t} for which no profiles can be found within the period \code{t-interval.max/2} to \code{t+interval.max/2}.
#' @inheritParams mtr
#' @export
#' @return atomic vector with (cumulative) migration traffic in number of individuals / km
#' @examples
#' # get example time series object:
#' data(example_vpts)
#' # print cumulative migration traffic to console:
#' cmt(example_vpts)
#' # plot cumulative migration traffic:
#' plot(cmt(example_vpts), type = "l", xlab = "time" , ylab = "CMT [birds/km]")
cmt <- function(x,alt.min=0, alt.max=Inf, alpha=NA,interval.max=Inf){
  stopifnot(inherits(x,"vpts"))
  .Deprecated("integrate_profile")
  .Deprecated(msg="'cmt' has been moved to the 'mt' column in the output of integrate_profile()")
  vintegrated=integrate_profile(x,alt.min,alt.max,alpha,interval.max)
  vintegrated$mt
}
