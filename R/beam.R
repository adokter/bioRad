#' Calculate radar beam height
#'
#' Calculates the height of a radar beam as a function of elevation and range,
#' assuming the beam is emitted at surface level.
#'
#' @param range numeric. Range (distance from the radar antenna) in m.
#' @param elev numeric. Elevation in degrees.
#' @param k Standard refraction coefficient.
#' @param lat Geodetic latitude in degrees.
#' @param re Earth equatorial radius in km.
#' @param rp Earth polar radius in km.
#'
#' @return numeric. Beam height in m.
#'
#' @export
#'
#' @details To account for refraction of the beam towards the earth's surface,
#' an effective earth's radius of k * (true radius) is assumed, with k = 4/3.
#'
#' The earth's radius is approximated as a point on a spheroid surface, with
#' \code{re} the longer equatorial radius, and \code{rp} the shorter polar
#' radius. Typically uncertainties in refraction coefficient are relatively
#' large, making oblateness of the earth and the dependence of earth radius with
#' latitude only a small correction. Using default values assumes an average
#' earth's radius of 6371 km.
beam_height <- function(range, elev, k = 4 / 3, lat = 35, re = 6378, rp = 6357) {
  assert_that(is.numeric(range))
  assert_that(is.numeric(elev))
  assert_that(is.number(k))
  assert_that(is.number(lat))
  assert_that(is.number(re))
  assert_that(is.number(rp))
  sqrt(range^2 + (k * earth_radius(re, rp, lat))^2 +
    2 * range * (k * earth_radius(re, rp, lat)) * sin(elev * pi / 180)) - k * earth_radius(re, rp, lat)
}

earth_radius <- function(a, b, lat) {
  lat <- lat * pi / 180
  1000*sqrt(((a^2 * cos(lat))^2 + (b^2 * sin(lat))^2) / ((a * cos(lat))^2 + (b * sin(lat))^2))
}

#' Calculate radar beam width
#'
#' Calculates the width of a radar beam as a function of range and beam angle.
#'
#' @param range numeric. Range (distance from the radar antenna) in m.
#' @param beam_angle numeric. Beam opening angle in degrees, typically the
#' the angle between the half-power (-3 dB) points of the main lobe
#'
#' @return numeric. Beam width in m.
#'
#' @export
beam_width <- function(range, beam_angle = 1) {
  assert_that(is.numeric(range))
  assert_that(is.number(beam_angle))
  range * sin(beam_angle * pi / 180)
}

#' Gaussian beam profile as a function of height relative to ground level
#'
#' Normalized altitudinal pattern of radiated energy as a function of
#' altitude at a given distance from the radar, assuming the beam is emitted at surface level.
#'
#' @param height numeric. Height above ground level in meter.
#' @param range numeric. Range (distance from the radar antenna) in meter.
#' @param elev numeric. Beam elevation in degrees.
#' @param k Standard refraction coefficient.
#' @param lat Geodetic latitude in degrees.
#' @param re Earth equatorial radius in km.
#' @param rp Earth polar radius in km.
#'
#' @return numeric.
#'
#' @details Beam profile is calculated using \link{beam_height} and \link{beam_width}.
#'
#' @keywords internal
#' @examples
#' # plot the beam profile of a beam emitted at 2 degrees elevation at 35000 meter from the radar:
#' plot(gaussian_beam_profile(0:3000,35000,2),0:3000,xlab="normalized radiated energy",ylab="altitude [m]",main="beam profile of 2 degree elevation beam at 35 km")
gaussian_beam_profile=function(height,range,elev,beam_angle=1, k=4/3, lat=35, re = 6378, rp = 6357) dnorm(height,mean=beam_height(range=range,elev=elev,k=k,lat=lat,re=re,rp=rp),sd=beam_width(range=range, beam_angle = beam_angle)/(2*sqrt(2*log(2))))

#' Calculate vertical radiation profile
#'
#' Calculate for a set of beam elevations elev
#' the altitudinal normalized distribution of radiated energy by those beams.
#' @param height numeric. Height(s) above ground level in meter.
#' @param range Distance from the radar (over ground level) for which to calculate the altitudinal beam profile in m.
#' @param elev numeric vector of radar beam elevation(s) in degrees.
#' @param beam_angle numeric. Beam opening angle in degrees, typically the
#' the angle between the half-power (-3 dB) points of the main lobe
#' @param NEZH The total system noise expressed as the horizontally-polarized reflectivity factor (dBZ)
#'  it would represent at one km distance from the radar. (not implemented yet)
#' @param LOG Security distance above mean noise level (dB) threshold value, i.e. the dB above noise
#'  for the signal processor to report a valid reflectivity value. (not implemented yet)
#' @param k Standard refraction coefficient.
#' @param lat Geodetic latitude in degrees.
#' @param re Earth equatorial radius in km.
#' @param rp Earth polar radius in km.
#' @return numeric vector. Normalized radiated energy at each of the specified heights.
#'
#' @export
#'
#' @details Beam profile is calculated using \link{beam_height} and \link{beam_width}.
#' Returns a beam profile as a function of height relative to ground level.
#'
#' Returns the normalized altitudinal pattern of radiated energy as a function of
#' altitude at a given distance from the radar, assuming the beams are emitted at surface level.
#'
#' @examples
#' plot(beam_profile(0:3000,35000,c(1,2)),0:3000,xlab="normalized radiated energy",ylab="altitude [m]",main="beam elevations: 1,2 degrees; range: 35 km")
beam_profile = function(height, range, elev, beam_angle=1, k=4/3, lat=35, re = 6378, rp = 6357, NEZH = NA, LOG = NA){
  assert_that(is.numeric(height))
  assert_that(is.numeric(range))
  assert_that(is.numeric(elev))
  assert_that(is.number(beam_angle))
  assert_that(is.number(k))
  assert_that(is.number(lat))
  assert_that(is.number(rp))
  assert_that(is.number(re))
  # calculate radiation pattern
  rowSums(do.call(cbind,lapply(elev,function(x) gaussian_beam_profile(height,range,x,beam_angle=beam_angle, lat=lat,k=k, re = re, rp = rp))))/length(elev)
}

# helper function for beam_profile_overlap()
beam_profile_overlap_help = function(pvol, vp, range, ylim=c(0,4000), steps=500,quantity="dens", normalize=TRUE){
  # define altitude grid
  height=seq(ylim[1],ylim[2],length.out=steps)
  # calculate altitudinal radiation pattern of all radar beams combined
  beamprof=beam_profile(height=height,range=range,elev=get_elevation_angles(pvol),lat=pvol$geo$lat)
  # normalize the distribution
  step=(ylim[2]-ylim[1])/(steps-1)
  if(normalize) beamprof=beamprof/sum(beamprof*step)
  # output as data.frame
  beamprof=data.frame(height=height,radiation=beamprof)
  # linearly interpolate the density of the vertical profile at the same grid as beamprof above
  beamprof$vpr=approxfun(vp$data$HGHT+vp$attributes$where$interval/2,vp$data[[quantity]])(height)
  # normalize the vertical profile density
  step=(ylim[2]-ylim[1])/(steps-1)
  beamprof$vpr=beamprof$vpr/sum(step*beamprof$vpr,na.rm=T)
  # calculate the Bhattacharyya coefficient of the density profile and radiation coverage pattern
  sum(step*sqrt(beamprof$radiation*beamprof$vpr),na.rm=T)
}

#' Calculate overlap between a vertical profile ('vp') and the vertical radiation profile
#'
#' Calculates the distribution overlap between a vertical profile ('vp')
#' and the vertical radiation profile as calculate with \link[beam_profile].
#' @param pvol a polar volume of class pvol
#' @param vp a vertical profile of class vp
#' @param range the distance(s) from the radar for which to calculate the overlap in m.
#' @param ylim altitude range in meter, given as a numeric vector of length two.
#' @param steps number of integration steps over altitude range ylim, defining altitude grid size used for numeric integrations
#' @param quantity profile quantity to use for the altitude distribution, one of 'dens' or 'eta'.
#' @param normalize Whether to normalize the radiation coverage pattern over the altitude range specified by ylim
#' @return A data.frame with columns range and overlap. Overlap is calculated as the
#' Bhattacharyya coefficient (i.e. distribution overlap) between the (normalized) vertical profile vp
#' and the (normalized) radiation coverage pattern as calculated by \link{beam_profile}
#'
#' @export
#'
#' @details to be written
#'
#' @examples
#' # locate example volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' # load the example polar volume file:
#' pvol <- read_pvolfile(pvolfile)
#' # let us use this example vertical profile:
#' example_vp
#' # calculate overlap between vertical profile of birds
#' # and the vertical radiation profile emitted by the radar:
#' bpo=beam_profile_overlap(pvol, example_vp, seq(0,100000,1000))
#' # plot the calculated overlap:
#' plot(bpo)
beam_profile_overlap = function(pvol, vp, range, ylim=c(0,4000), steps=500, quantity="dens",normalize=T){
  if(!is.pvol(pvol)) stop("'pvol' should be an object of class pvol")
  if(!is.vp(vp)) stop("'vp' should be an object of class vp")
  if(!is.numeric(range) | min(range)<0) stop("'range' should be a positive numeric value or vector")
  if(length(ylim)!=2 & !is.numeric(ylim)) stop("'ylim' should be a numeric vector of length two")
  if(is.na(ylim[1]) | is.na(ylim[2]) | ylim[1]>ylim[2]) stop("'ylim' should be a vector with two numeric values for upper and lower bound")
  if(length(steps)!=1 & !is.numeric(steps)) stop("'step' should be a numeric value")
  if(!(quantity %in% c("dens","eta"))) stop("'quantity' should be one of 'dens' or 'eta'")
  overlap=sapply(range, function(x) beam_profile_overlap_help(pvol,vp, x, ylim=ylim, steps=steps, quantity=quantity,normalize=normalize))
  data.frame(range=range,overlap=overlap)
}

