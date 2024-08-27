#' Partition mixtures of animals using assumptions on airspeeds.
#'
#' Partition mixtures of animals using assumptions on airspeeds.
#' @param x a `vp` or `vpts` object.
#' @param ... `eta`, `u`, `v`, `U`, `V` arguments, taken from object for `vp` or `vpts` class.
#' @param eta a mixture animal density or linear reflectivity eta.
#' @param u the mixture's ground speed u component (west to east)
#' @param v the mixture's ground speed v component (south to north)
#' @param U the west to east wind component
#' @param V the south to north wind component
#' @param fast the fast component's airspeed
#' @param slow the slow component's airspeed
#' @param drop_slow_component when TRUE (default) output density, ground speed and
#' heading for fast component, when FALSE for slow component.
#' @param drop_missing Values `x` without an associated ground speed
#' and wind speed are set to NA when `TRUE`, or returned unaltered when `FALSE` (default).
#' @param keep_mixture When `TRUE` store original mixture reflectivity and speeds as
#' renamed quantities with `mixture_` prefix
#' @return a data.frame with corrected density or reflectivity x, ground speed (u, v)
#' and heading in clockwise degrees from north.
#' @export
#' @name clean_mixture
#' @examples
#' # drop the slow component (typically insects)
#' clean_mixture(100,-13,13,-7,6, fast=12, slow=1)
#' # drop the fast component (typically birds)
#' clean_mixture(100,-13,13,-7,6, fast=12, slow=1, drop_slow_component=FALSE)
NULL

#' @rdname clean_mixture
#'
#' @export
clean_mixture <- function(x, ..., slow = 1, fast = 8, drop_slow_component = TRUE, drop_missing = FALSE, keep_mixture = FALSE){
  UseMethod("clean_mixture", x)
}

#' @rdname clean_mixture
#'
#' @export
clean_mixture.default <- function(eta, u, v, U, V, slow = 1, fast = 8, drop_slow_component = TRUE, drop_missing = FALSE, keep_mixture = FALSE){
  # verify input
  assertthat::assert_that(all(eta >= 0, na.rm = TRUE))
  assertthat::assert_that(is.numeric(u))
  assertthat::assert_that(is.numeric(v))
  assertthat::assert_that(is.numeric(U))
  assertthat::assert_that(is.numeric(V))
  assertthat::assert_that(slow >= 0)
  assertthat::assert_that(fast > 0)
  assertthat::assert_that(fast > slow)
  assertthat::assert_that(assertthat::is.flag(drop_slow_component))
  assertthat::assert_that(assertthat::is.flag(drop_missing))
  assertthat::assert_that(assertthat::is.flag(keep_mixture))

  if(!all(is.finite(U))) warning("U component of wind contains non finite values")
  if(!all(is.finite(V))) warning("V component of wind contains non finite values")

  # define helper quantities:
  wind_speed <- sqrt(U^2 + V^2)
  wind_direction <- atan2(V,U)
  mixture_airspeed <- sqrt((u-U)^2 + (v-V)^2)
  mixture_heading <- (pi / 2 - atan2(v-V, u-U)) * 180 / pi
  mixture_heading[which(mixture_heading<0)]=mixture_heading[which(mixture_heading<0)]+360

  # solve mixture equations for slow proportion f
  p1 <- slow^2 - fast^2
  p2 <- 2*fast^2 - 2*(slow/wind_speed)*(u*U+v*V - wind_speed^2)
  p3 <- (u-U)^2 + (v-V)^2 - fast^2
  f <- (-p2+sqrt(p2^2-4*p1*p3))/(2*p1)

  # catch limiting cases for which f is not defined
  idx_f_zero <- mixture_airspeed > fast
  if(length(which(idx_f_zero))>0){
    warning("Assigning all reflectivity to fast component for mixture airspeeds exceeding airspeed of fast component")
    f[idx_f_zero]=0
  }
  idx_f_one <- mixture_airspeed < slow
  if(length(which(idx_f_one))>0){
    warning("Assigning all reflectivity to slow component for mixture airspeeds below airspeed of slow component")
    f[idx_f_one]=1
  }

  # initialize corrected eta and speed matrix or vector:
  eta_corr=eta
  air_u = u-U
  air_v = v-V

  # initialize and define indices to correct densities and speeds
  idx = !is.na(f) & !idx_f_zero & !idx_f_one

  if(drop_slow_component){
    # fast component airspeed, typically birds:
    eta_corr[idx]=((1-f)*eta)[idx]
    air_u[idx]=(((u-U)-(slow/wind_speed)*U*f)/(1-f))[idx]
    air_v[idx]=(((v-V)-(slow/wind_speed)*V*f)/(1-f))[idx]
  } else{
    # slow component airspeed, typically insects:
    eta_corr[idx]=(f*eta)[idx]
    air_u[idx]=slow*cos(wind_direction)[idx]
    air_v[idx]=slow*sin(wind_direction)[idx]
  }

  if(drop_missing){
    eta_corr[is.na(f)]=NaN
    air_u[is.na(f)]=NaN
    air_v[is.na(f)]=NaN
  }

  # calculate speed and direction
  ff <- sqrt((U+air_u)^2 + (V+air_v)^2)
  dd <- (pi / 2 - atan2(V+air_v, U+air_u)) * 180 / pi
  dd[which(dd<0)]=dd[which(dd<0)]+360

  output <- list(eta=eta_corr,u=U+air_u,v=V+air_v, airspeed = sqrt(air_u^2+air_v^2), heading=(pi/2-atan2(air_v,air_u))*180/pi,
                 airspeed_u = air_u, airspeed_v = air_v, f=f)

  if(keep_mixture){
    output = append(output, list(mixture_eta = eta, mixture_u=u, mixture_v=v, mixture_airspeed=mixture_airspeed, mixture_heading=mixture_heading))
  }

  output
}

#' @rdname clean_mixture
#'
#' @export
clean_mixture.vpts <- function(x, ..., slow = 1, fast = 8, drop_slow_component = TRUE, drop_missing = FALSE, keep_mixture = FALSE){
  assertthat::assert_that(inherits(x,"vpts") | inherits(x,"vp"))
  assertthat::assert_that(all(c("u_wind","v_wind") %in% names(x$data)), msg="function requires paired wind data, quantities `u_wind` and `v_wind` not found")
  assertthat::assert_that(assertthat::is.number(x$attributes$how$rcs_bird), msg="radar cross section not defined, please set with `rcs()`")

  # call function
  result <- clean_mixture.default(x$data$eta, x$data$u, x$data$v, x$data$u_wind, x$data$v_wind,
                                  slow = slow, fast = fast, drop_slow_component = drop_slow_component,
                                  drop_missing = drop_missing, keep_mixture = keep_mixture)

  # map results to vp or vpts structure
  x$data$eta=result$eta
  x$data$u=result$u
  x$data$v=result$v
  x$data$airspeed=result$airspeed
  x$data$heading=result$heading
  x$data$airspeed_u=result$airspeed_u
  x$data$airspeed_v=result$airspeed_v
  x$data$proportion_slow=result$f
  if(keep_mixture){
    x$data$mixture_eta=result$mixture_eta
    x$data$mixture_u=result$mixture_u
    x$data$mixture_v=result$mixture_v
    x$data$mixture_airspeed=result$mixture_airspeed
    x$data$mixture_heading=result$mixture_heading
  }

  # trigger rebuild of dens from eta:
  rcs(x) <- rcs(x)

  # return object
  x
}

#' @rdname clean_mixture
#'
#' @export
clean_mixture.vp <- function(x, ..., slow = 1, fast = 8, drop_slow_component = TRUE, drop_missing = FALSE, keep_mixture = FALSE){
  assertthat::assert_that(inherits(x,"vp"))

  clean_mixture.vpts(x$data$eta, x$data$u, x$data$v, x$data$u_wind, x$data$v_wind,
                        slow = slow, fast = fast, drop_slow_component = drop_slow_component,
                        drop_missing = drop_missing, keep_mixture = keep_mixture)
}
