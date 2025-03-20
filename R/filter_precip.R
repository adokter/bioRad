#' Posthoc precipitation filter
#'
#' The posthoc precipitation filter assesses how much of the altitude column has a high
#' total reflectivity factor (biology + meteorology) consistent with precipitation,
#' and removes biological signals when there is evidence for the presence of precipitation.
#' Applied to vertical profiles ('vp') or time series of vertical profiles ('vpts').
#'
#' @param x A `vp` or `vpts` object.
#' @param dbz The minimum reflectivity factor for precipitation.
#' @param range The minimum altitude range with total reflectivity factor.
#' `DBZH` > `dbz` at which the filter is triggered.
#' @param alt_max Maximum altitude above ground level to consider.
#' @param drop When `TRUE` the profile is removed from the
#' @return A `vpts` object or a `vp` object, depending on input `x`.
#'
#' @export
#' @details
#' During precipitation events usually a high proportion of the altitude
#' column will show a high total reflectivity `DBZH` (which includes biology +
#' meteorology), because precipitation usually falls from several kilometers high
#' to the ground surface. Precipitation events are often obvious in profile plots
#' of quantity `DBZH` as reflectivity signals extending from ground level to high
#' altitudes far above the typical altitudes where biology is expected. This filter
#' identifies and removes these cases.
#'
#' The posthoc precipitation filter examines the total reflectivity factor `DBZH`
#' and calculates the altitude range at which `DBZH` is larger than parameter
#' `dbz`. Whenever this altitude range is larger than parameter `range` (and `drop`
#' is `FALSE`), the biology is removed by setting profile quantities `dens` and `eta`
#' to zero and profile quantity `dbz` to `-Inf`. When parameter `drop`
#' is `TRUE`, the profile is removed from the time series altogether.
#'
#' This posthoc precipitation filter is likely to remove biological scatterers
#' that co-occur with precipitation, for example biological movements during isolated
#' convective thunderstorms. It is more aggressive than precipitation filters
#' applied during vertical profile generation with `calculate_vp()` that attempt to
#' remove precipitation and retain biology. The posthoc precipitation filter is especially
#' useful for analyses where you want to minimize the risk of precipitation contamination,
#' at the cost of filtering out some biology during precipitation events.
#'
#' Lowering the default minimum reflectivity (`dbz`) for precipitation
#' below 7 dBZ is not recommended, as most precipitation has a reflectivity above 7 dBZ.
#'
#' Parameter `range` should be chosen carefully, and should be higher than the
#' typical altitude where biological scatterers still reach a reflectivity factor equal to `dbz`.
#'
#' Note that at S-band wavelengths bird migration occurs much more frequently in the reflectivity
#' regime for precipitation than at C-band. Therefore, at C-band lower settings for parameter `dbz`
#' are appropriate than at S-band.
#'
#' @seealso
#' * [eta_to_dbz]
#' * [dbz_to_eta]
#'
#' @examples
#' # rain periods are visible in quantity DBZH as dark vertical lines
#' # extending above 3 km height:
#' plot(regularize_vpts(example_vpts), quantity='DBZH')
#' # Apply posthoc filter to remove profiles during precipitation events:
#' # (regularization is applied to visualize removed profiles)
#' my_vpts <- regularize_vpts(filter_precip(example_vpts, drop=TRUE))
#' # verify that rain events have been removed:
#' plot(my_vpts, quantity='DBZH')
#' # the posthoc filter removes some biology during precipitation:
#' plot(my_vpts, quantity='dens')
#' # original retains more biology (but at the cost of a higher
#' # likelihood of occasional precipitation contamination):
#' plot(regularize_vpts(example_vpts), quantity='dens')
#' # filter can also be applied to single vp objects:
#' filter_precip(example_vp)
filter_precip <- function(x, dbz=7, range=2500, alt_max=3000, drop=FALSE){
  assertthat::assert_that(is.vp(x) | is.vpts(x))
  assertthat::assert_that(assertthat::is.number(dbz))
  assertthat::assert_that(assertthat::is.number(range))
  assertthat::assert_that(assertthat::is.number(alt_max))
  assertthat::assert_that(alt_max>0)
  assertthat::assert_that(assertthat::is.flag(drop))
  assertthat::assert_that(range<=alt_max)
  assertthat::assert_that(!(drop & is.vp(x)), msg='parameter `drop` should be `TRUE` for objects of class `vp`')
  if(dbz<7) warning("dbz value too low for typical precipitation")
  height_index_max <- ((x$attributes$where$height + alt_max) %/% x$attributes$where$interval)
  height_index_max <- min(x$attributes$where$levels,height_index_max)
  if(is.vpts(x)){
    height_range <- colSums(x$data$DBZH[1:height_index_max,]>dbz,na.rm=T)*x$attributes$where$interval
  } else{
    height_range <- sum(x$data$DBZH[1:height_index_max]>dbz,na.rm=T)*x$attributes$where$interval
  }

  index <- which(height_range > range)
  if(length(index)==0) return(x)
  # if remove, drop the profiles
  if(drop) return(x[-index])
  # otherwise set the density field to zero, but keep the profile
  if(is.vpts(x)){
    x$data$dens[,index] <- 0
    x$data$eta[,index] <- 0
    x$data$dbz[,index] <- -Inf
  } else{
    x$data$dens[index] <- 0
    x$data$eta[index] <- 0
    x$data$dbz[index] <- -Inf
  }

  x
}
