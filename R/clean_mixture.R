#' Partition mixtures of animals using assumptions on airspeeds.
#'
#' Partition mixtures of animals using assumptions on airspeeds.
#' @param x a mixture animal density or linear reflectivity eta.
#' @param u the mixture's ground speed u component (west to east)
#' @param v the mixture's ground speed v component (south to north)
#' @param U the west to east wind component
#' @param V the south to north wind component
#' @param fast the fast component's airspeed
#' @param slow the slow component's airspeed
#' @param drop_slow_component when TRUE (default) output density, ground speed and
#' heading for fast component, when FALSE for slow component.
#' @return a data.frame with corrected density or reflectivity x, ground speed (u, v)
#' and heading in clockwise degrees from north.
#' @export
#' @examples
#' # drop the slow component (typically insects)
#' clean_mixture(100,-13,13,-7,6, fast=12, slow=1)
#' # drop the fast component (typically birds)
#' clean_mixture(100,-13,13,-7,6, fast=12, slow=1, drop_slow_component=FALSE)
clean_mixture <- function(x, u, v, U, V, slow = 1, fast = 12, drop_slow_component = TRUE){
  # verify input
  assertthat::assert_that(x >= 0)
  assertthat::assert_that(assertthat::is.number(u))
  assertthat::assert_that(assertthat::is.number(v))
  assertthat::assert_that(assertthat::is.number(U))
  assertthat::assert_that(assertthat::is.number(V))
  assertthat::assert_that(slow >= 0)
  assertthat::assert_that(fast > 0)
  assertthat::assert_that(fast > slow)
  assertthat::assert_that(is.flag(drop_slow_component))

  # define helper quantities:
  wind_speed <- sqrt(U^2 + V^2)
  wind_direction <- atan2(V,U)
  mixture_airspeed <- sqrt((u-U)^2 + (v-V)^2)

  # catch limiting cases
  if(mixture_airspeed > fast){
    warning("Airspeed of mixture exceeds airspeed of fast component, assigning all weight to fast component")
    f=0
  }
  if(mixture_airspeed < slow){
    warning("Airspeed of slow component exceeds airspeed of mixture, assigning all weight to slow component")
    f=1
  }
  if(mixture_airspeed <= fast & mixture_airspeed >= slow){
    p1 <- slow^2 - fast^2
    p2 <- 2*(wind_speed + fast^2 - slow*(u*U+v*V)/wind_speed)
    p3 <- (u-U)^2 + (v-V)^2 - fast^2
    # signal proportion attributed to the slow component:
    f <- (-p2+sqrt(p2^2-4*p1*p3))/(2*p1)
  }

  if(drop_slow_component){
    # fast component airspeed, typically birds:
    if(f==1){
      air_u=NA
      air_v=NA
    } else{
      air_u=((u-U)-(slow/wind_speed)*U*f)/(1-f)
      air_v=((v-V)-(slow/wind_speed)*V*f)/(1-f)
    }
    x_corr=(1-f)*x
  } else{
    # slow component airspeed, typically insects:
    air_u=slow*cos(wind_direction)
    air_v=slow*sin(wind_direction)
    x_corr=(f*x)
  }

  data.frame(x=x_corr,u=U+air_u,v=V+air_v, heading=(pi/2-atan2(air_v,air_u))*180/pi)
}


