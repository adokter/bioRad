#' Partition mixtures of animals using assumptions on airspeeds.
#'
#' Partition mixtures of birds and insects using assumptions on their respective airspeeds,
#' following the approach by Shi et al. (2025).
#' @param x a `vp` or `vpts` object, or a mixture animal density or linear reflectivity eta in cm\eqn{^2}/km\eqn{^3}.
#' @param ... `eta`, `u`, `v`, `u_wind`, `v_wind` arguments, taken from object for `vp` or `vpts` class.
#' @param u the mixture's ground speed u component (west to east) in m/s.
#' @param v the mixture's ground speed v component (south to north) in m/s.
#' @param u_wind the west to east wind component in m/s. In the case of `vp` and `vpts` objects
#' the quantity name for the U-component of the wind.
#' @param v_wind the south to north wind component in m/s.  In the case of `vp` and `vpts` objects
#' the quantity name for the V-component of the wind.
#' @param fast the fast component's airspeed in m/s, typically the airspeed of insects.
#' Either a single number, or (optionally for `vpts`) a numeric vector equal in length to the number of profiles,
#' or a data column name (see Details).
#' @param slow the slow component's airspeed in m/s, typically the airspeed of birds.
#' Either a single number, or (optionally for `vpts`) a numeric vector equal in length to the number of profiles,
#' or a data column name (see Details).
#' @param drop_slow_component when TRUE (default) output density, ground speed and
#' heading for fast component, when FALSE for slow component.
#' @param drop_missing Values `eta` without an associated ground speed
#' and wind speed are set to NA when `TRUE`, or returned unaltered when `FALSE` (default).
#' @param keep_mixture When `TRUE` store original mixture reflectivity and speeds as
#' renamed quantities with `mixture_` prefix
#' @return a named list with cleaned densities and speeds.
#' Output differs depending on whether the fast component is retained
#' (`drop_slow_component`=`TRUE`, default) or the slow component (`drop_slow_component`=`FALSE`, default). Output quantities include:
#'   * `eta`: cleaned reflectivity in cm^2/km^3.
#'   only the fast component (default) or the slow component
#'    (when `drop_slow_component` is `TRUE`).
#'   * `u`: cleaned ground speed component west to east in m/s.
#'   * `v`: cleaned ground speed component south to north in m/s.
#'   * `airspeed`: the airspeed of the selected component in m/s.
#'   * `airspeed_u`: the u-component (west to east) of the airspeed of the retained component in m/s.
#'   * `airspeed_v`: the v-component (south to north) of the airspeed of the retained component in m/s.
#'   * `heading`: the heading of the selected component in degrees clockwise from north.
#'   * `f`: the reflectivity proportion of the slow component (0-1 range), typically the proportion of insects.
#'
#' For `vp` and `vpts` objects the quantities `eta`,`u`,`v` will be updated, and other
#' quantities listed above will be added.
#' @export
#' @name clean_mixture
#' @examples
#' # convert profile object to data.frame
#' df <- as.data.frame(example_vp, suntime=FALSE)
#' # add wind u and v component wind data
#' # (here a NW wind identical at all altitudes)
#' df$u_wind=3
#' df$v_wind=-3
#' # convert back to vp object
#' my_vp <- as.vp(df)
#' # partition the mixture:
#' my_vp_clean <- clean_mixture(my_vp)
#'
#' # drop the slow component (typically insects)
#' clean_mixture(100,u=-13,v=13,u_wind=-7,v_wind=6, fast=8, slow=1)
#' # drop the fast component (typically birds)
#' clean_mixture(100,u=-13,v=13,u_wind=-7,v_wind=6, fast=8, slow=1, drop_slow_component=FALSE)
#' # keep the original mixture reflectivity and speed components
#' clean_mixture(100,u=-13,v=13,u_wind=-7,v_wind=6, fast=8, slow=1, keep_mixture=TRUE)
#' # keep reflectivity unaltered when one of the speed components is not a number:
#' clean_mixture(100,u=-13,v=13,u_wind=NaN,v_wind=6, fast=8, slow=1)["eta"]
#' # set reflectivity to NaN when one of the speed components is not a number:
#' clean_mixture(100,u=-13,v=13,u_wind=NaN,v_wind=6, fast=8, slow=1, drop_missing=TRUE)["eta"]
#' @references
#' * Shi X, Drucker J, Chapman JW, Sanchez Herrera M, Dokter AM
#' Analysis of mixtures of birds and insects in weather radar data.
#' Ornithological Applications. 2025 (in press) \doi{10.1093/ornithapp/duaf020}.
#' * Nussbaumer R, Schmid B, Bauer S, Liechti F.
#' A Gaussian mixture model to separate birds and insects in
#' single-polarization weather radar data. Remote Sensing. 2021 May 19;13(10):1989 \doi{10.3390/rs13101989}.
#' @details
#' For a detail description of the methodology see Shi et al. (2025).
#' Most commonly the fast component refers to migrating birds, while
#' the slow component refers to insects. The slow component is always
#' oriented in the direction of the wind by definition.
#' Note that for mixture airspeeds exceeding the airspeed of the fast component,
#' all reflectivity is assigned to the fast component. Similarly, for mixture
#' airspeeds below the airspeed of the slow component, all reflectivity
#' will be assigned to the slow component.
#'
#'
#' ## How to use this function?
#' 1. To apply this function to `vp` or `vpts` data altitudinal wind data
#' needs to be added to the vertical profile data first. This is most easily
#' accomplished by first converting the objects to a `data.frame` with [as.vp()] or [as.vpts()].
#' Wind data can then be added as a new columns to the data.frame.
#' By default the wind data is expected to be named `u_wind` for the U component
#' and `v_wind` for the V component of the wind.
#' Alternatively, arguments `u_wind` and `v_wind` can be used to specify different names.
#' 2. Realistic assumptions for the expected airspeed for the slow (insect)
#' and fast (bird) components need to be provided, using arguments `slow` and `fast`.
#' See Shi et al. 2025 for recommendations in choosing these values.
#' The parameter values for `fast` and `slow` can be specified as follows:
#'     * as single values applied to all heights and timestamps
#'     * as a numeric vector of equal length as the number of profiles in the `vpts`,
#'       allowing the user to specify changes in the parameter over time
#'     * as the name of a profile data quantity, allowing the user to specify changes in
#'       the parameter over time and/or altitude. Profile quantities are most easily added
#'       by first converting the `vpts` object to a data.frame with [as.data.frame.vpts()],
#'       adding the values, and back-converting with [as.vpts]
#' 3. Use `drop_slow_component` to toggle between retaining the slow or fast component.
#' When `TRUE` the fast (bird) component is retained. When `FALSE` the slow (insect)
#' component is retained. Note that in this case the corrected ground speed direction will be
#' identical to the wind direction, and the magnitude of the ground speed will be equal
#' to the wind speed plus the value of `slow`, due to the underlying assumption
#' of wind following by the slow component.
NULL

#' @rdname clean_mixture
#'
#' @export
clean_mixture <- function(x, ...){
  UseMethod("clean_mixture", x)
}

#' @rdname clean_mixture
#'
#' @export
clean_mixture.default <- function(x, slow = 1, fast = 8, drop_slow_component = TRUE, drop_missing = FALSE, keep_mixture = FALSE, u_wind, v_wind, u, v, ...){
  # verify input
  assertthat::assert_that(all(x >= 0, na.rm = TRUE))
  assertthat::assert_that(is.numeric(u))
  assertthat::assert_that(is.numeric(v))
  assertthat::assert_that(is.numeric(u_wind))
  assertthat::assert_that(is.numeric(v_wind))
  assertthat::assert_that(is.numeric(slow))
  assertthat::assert_that(is.numeric(fast))
  assertthat::assert_that(all(slow >= 0), msg="value(s) `slow` should be >= 0")
  assertthat::assert_that(all(fast > 0), msg="value(s) `fast` should be > 0")
  assertthat::assert_that(all(fast > slow), msg="value(s) `slow` should be smaller than `fast`")
  assertthat::assert_that(assertthat::is.flag(drop_slow_component))
  assertthat::assert_that(assertthat::is.flag(drop_missing))
  assertthat::assert_that(assertthat::is.flag(keep_mixture))

  if(!all(is.finite(u_wind))) warning("U component of wind contains non finite values")
  if(!all(is.finite(v_wind))) warning("V component of wind contains non finite values")

  # define helper quantities:
  wind_speed <- sqrt(u_wind^2 + v_wind^2)
  wind_direction <- atan2(v_wind,u_wind)
  mixture_airspeed <- sqrt((u-u_wind)^2 + (v-v_wind)^2)
  mixture_heading <- (pi / 2 - atan2(v-v_wind, u-u_wind)) * 180 / pi
  mixture_heading[which(mixture_heading<0)]=mixture_heading[which(mixture_heading<0)]+360

  # solve mixture equations for slow proportion f
  p1 <- slow^2 - fast^2
  p2 <- 2*fast^2 - 2*(slow/wind_speed)*(u*u_wind+v*v_wind - wind_speed^2)
  p3 <- (u-u_wind)^2 + (v-v_wind)^2 - fast^2
  f <- (-p2+sqrt(p2^2-4*p1*p3))/(2*p1)

  # catch limiting cases for which f is not defined
  idx_f_zero <- mixture_airspeed > fast
  if(length(which(idx_f_zero))>0){
    #warning("Assigning all reflectivity to fast component for mixture airspeeds exceeding airspeed of fast component")
    f[idx_f_zero]=0
  }
  idx_f_one <- mixture_airspeed < slow
  if(length(which(idx_f_one))>0){
    #warning("Assigning all reflectivity to slow component for mixture airspeeds below airspeed of slow component")
    f[idx_f_one]=1
  }

  # initialize corrected eta and speed matrix or vector:
  eta_corr=x
  air_u = u-u_wind
  air_v = v-v_wind

  # initialize and define indices to correct densities and speeds
  idx = !is.na(f) & !idx_f_zero & !idx_f_one

  if(drop_slow_component){
    # fast component airspeed, typically birds:
    eta_corr[idx]=((1-f)*x)[idx]
    air_u[idx]=(((u-u_wind)-(slow/wind_speed)*u_wind*f)/(1-f))[idx]
    air_v[idx]=(((v-v_wind)-(slow/wind_speed)*v_wind*f)/(1-f))[idx]
  } else{
    # slow component airspeed, typically insects:
    eta_corr[idx]=(f*x)[idx]
    air_u[idx]=slow*cos(wind_direction)[idx]
    air_v[idx]=slow*sin(wind_direction)[idx]
  }

  if(drop_missing){
    eta_corr[is.na(f)]=NaN
    air_u[is.na(f)]=NaN
    air_v[is.na(f)]=NaN
  }

  # calculate speed and direction
  ff <- sqrt((u_wind+air_u)^2 + (v_wind+air_v)^2)
  dd <- (pi / 2 - atan2(v_wind+air_v, u_wind+air_u)) * 180 / pi
  dd[which(dd<0)]=dd[which(dd<0)]+360

  output <- list(eta=eta_corr,u=u_wind+air_u,v=v_wind+air_v, airspeed = sqrt(air_u^2+air_v^2), heading=(pi/2-atan2(air_v,air_u))*180/pi,
                 airspeed_u = air_u, airspeed_v = air_v, f=f)

  if(keep_mixture){
    output = append(output, list(mixture_eta = x, mixture_u=u, mixture_v=v, mixture_airspeed=mixture_airspeed, mixture_heading=mixture_heading))
  }

  output
}

#' @rdname clean_mixture
#'
#' @export
clean_mixture.vpts <- function(x, slow = 1, fast = 8, drop_slow_component = TRUE, drop_missing = FALSE, keep_mixture = FALSE, u_wind="u_wind", v_wind="v_wind", ...){
  assertthat::assert_that(inherits(x,"vpts") | inherits(x,"vp"))
  if(inherits(x,"vpts") | inherits(x,"vp")){
    assertthat::assert_that(is.character(u_wind))
    assertthat::assert_that(is.character(v_wind))
  }
  if(inherits(x,"vpts")){
    n_profiles = dim(x)[1]
    n_bins = dim(x)[2]
  }
  else{ # x is a vp
    n_profiles = 1
    n_bins = dim(x)[1]
  }
  assertthat::assert_that(all(c(u_wind,v_wind) %in% names(x$data)), msg=paste0("function requires paired wind data, profile quantities `",u_wind,"` and/or `",v_wind,"` not found."))
  assertthat::assert_that(all(c("u","v") %in% names(x$data)), msg="function requires ground speed data, profile quantities `u` and/or `v` not found.")
  assertthat::assert_that("eta" %in% names(x$data), msg="function requires linear reflectivity data, profile quantity `eta` not found.")
  assertthat::assert_that(assertthat::is.number(x$attributes$how$rcs_bird), msg="radar cross section not defined, please set with `rcs()`.")

  if(is.character(slow)){
    assertthat::assert_that(slow %in% names(x$data), msg=paste0("value of argument `slow` (",slow,") not refering to a valid data column"))
    slow=x$data[[slow]]
  }
  else{
    assertthat::assert_that(is.numeric(slow))
    assertthat::assert_that(is.vector(slow))
    if(!assertthat::is.number(slow)){
      assertthat::assert_that(length(slow) == n_profiles, msg="`slow` should be a number or a vector equal to the number of profiles")
      slow=matrix(rep(slow,n_bins), nrow=n_bins, byrow=TRUE)
    }
  }
  if(is.character(fast)){
    assertthat::assert_that(fast %in% names(x$data), msg=paste0("value of argument `fast` (",fast,") not refering to a valid data column"))
    fast=x$data[[fast]]
  }
  else{
    assertthat::assert_that(is.numeric(fast))
    assertthat::assert_that(is.vector(fast))
    if(!assertthat::is.number(fast)){
      assertthat::assert_that(length(fast) == n_profiles, msg="`fast` should be a number or a vector equal to the number of profiles")
      fast=matrix(rep(fast,n_bins), nrow=n_bins, byrow=TRUE)
    }
  }

  # call function
  result <- clean_mixture.default(x$data$eta,
                                  slow = slow, fast = fast, drop_slow_component = drop_slow_component,
                                  drop_missing = drop_missing, keep_mixture = keep_mixture,
                                  x$data[[u_wind]], x$data[[v_wind]], x$data$u, x$data$v)

  # map results to vp or vpts structure
  x$data$eta=result$eta
  x$data$u=result$u
  x$data$v=result$v

  # check for quantities potentially overwritten
  quantities <- c("airspeed","airspeed_u","airspeed_v","heading","f")
  if(keep_mixture) quantities <- c(quantities, "mixture_eta","mixture_u","mixture_v","mixture_airspeed","mixture_heading")
  presence_test <- quantities %in% names(x$data)
  if(sum(presence_test)>0) warning(paste0("Overwriting existing quantities `", paste(quantities[presence_test], collapse="`, `"),"`."))

  x$data$airspeed=result$airspeed
  x$data$heading=result$heading
  x$data$airspeed_u=result$airspeed_u
  x$data$airspeed_v=result$airspeed_v
  x$data$f=result$f
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
clean_mixture.vp <- function(x, ..., slow = 1, fast = 8, drop_slow_component = TRUE, drop_missing = FALSE, keep_mixture = FALSE, u_wind="u_wind", v_wind="v_wind"){
  assertthat::assert_that(inherits(x,"vp"))

  clean_mixture.vpts(x,slow = slow, fast = fast, drop_slow_component = drop_slow_component,
                     drop_missing = drop_missing, keep_mixture = keep_mixture, u_wind=u_wind, v_wind=v_wind)
}
