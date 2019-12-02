#' Calculate radar beam height
#'
#' Calculates the height of a radar beam as a function of elevation and range,
#' assuming the beam is emitted at surface level.
#'
#' @param range numeric. Slant range in m, the length of the skywave path
#'  between target and the radar antenna.
#' @param elev numeric. Beam elevation in degrees.
#' @param k Standard refraction coefficient.
#' @param lat Geodetic latitude of the radar in degrees.
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
beam_height <- function(range, elev, k = 4 / 3, lat = 35, re = 6378,
                        rp = 6357) {
  assert_that(is.numeric(range))
  assert_that(is.numeric(elev))
  assert_that(is.number(k))
  assert_that(is.number(lat))
  assert_that(is.number(re))
  assert_that(is.number(rp))
  sqrt(
    range^2 + (k * earth_radius(re, rp, lat))^2 +
      2 * range * (k * earth_radius(re, rp, lat)) * sin(elev * pi / 180)
  ) - k * earth_radius(re, rp, lat)
}

earth_radius <- function(a, b, lat) {
  lat <- lat * pi / 180
  1000 * sqrt(
    ((a^2 * cos(lat))^2 + (b^2 * sin(lat))^2) /
      ((a * cos(lat))^2 + (b * sin(lat))^2)
  )
}

#' Calculate radar beam width
#'
#' Calculates the width of a radar beam as a function of range and beam angle.
#'
#' @param range numeric. Range (distance from the radar antenna) in m.
#' @param beam_angle numeric. Beam opening angle in degrees, typically the
#' angle between the half-power (-3 dB) points of the main lobe
#'
#' @return numeric. Beam width in m.
#'
#' @export
beam_width <- function(range, beam_angle = 1) {
  assert_that(is.numeric(range))
  assert_that(is.number(beam_angle))
  range * sin(beam_angle * pi / 180)
}

#' Gaussian beam profile as a function of height
#'
#' Normalized altitudinal pattern of radiated energy as a function of
#' altitude at a given distance from the radar.
#'
#' @inheritParams beam_height
#' @inheritParams beam_width
#' @param antenna numeric. Height of the centre of the radar antenna in meters
#' @param height numeric. Height in meter.
#'
#' @return numeric.
#'
#' @details Beam profile is calculated using \link{beam_height} and \link{beam_width}. \code{height} and
#' \code{antenna} should be given in reference to the same reference plane (e.g. ground level or sea level)
#'
#' @keywords internal
gaussian_beam_profile <- function(height, range, elev, antenna = 0,
                                  beam_angle = 1, k = 4 / 3, lat = 35, re = 6378,
                                  rp = 6357) {
  assert_that(is.numeric(height))
  assert_that(is.numeric(range))
  assert_that(is.number(elev))
  assert_that(is.number(antenna))
  assert_that(is.number(beam_angle))
  assert_that(is.number(k))
  assert_that(is.number(lat))
  assert_that(is.number(rp))
  assert_that(is.number(re))
  dnorm(
    height,
    mean = antenna + beam_height(
      range = range, elev = elev, k = k,
      lat = lat, re = re, rp = rp
    ), sd = beam_width(
      range = range, beam_angle =
        beam_angle
    ) / (2 * sqrt(2 * log(2)))
  )
}

#' Calculate vertical radiation profile
#'
#' Calculate for a set of beam elevations elev
#' the altitudinal normalized distribution of radiated energy by those beams.
#' @inheritParams gaussian_beam_profile
#' @inheritParams beam_range
#' @param elev numeric vector. Beam elevation(s) in degrees.
#' @return numeric vector. Normalized radiated energy at each of the specified
#'   heights.
#'
#' @export
#'
#' @details Beam profile is calculated using \link{beam_height} and
#'   \link{beam_width}. Returns a beam profile as a function of height relative
#'   to ground level.
#'
#'   Returns the normalized altitudinal pattern of radiated energy as a function
#'   of altitude at a given distance from the radar, assuming the beams are
#'   emitted at antenna level.
#'
#' @examples
#' plot(beam_profile(0:3000, 35000, c(1, 2)), 0:3000,
#'   xlab = "normalized radiated energy",
#'   ylab = "altitude [m]", main = "beam elevations: 1,2 degrees; distance: 35 km"
#' )
beam_profile <- function(height, distance, elev, antenna = 0, beam_angle = 1,
                         k = 4 / 3, lat = 35, re = 6378, rp = 6357) {
  assert_that(is.numeric(height))
  assert_that(is.numeric(distance))
  assert_that(is.numeric(elev))
  assert_that(is.number(antenna))
  assert_that(is.number(beam_angle))
  assert_that(is.number(k))
  assert_that(is.number(lat))
  assert_that(is.number(rp))
  assert_that(is.number(re))

  # calculate radiation pattern
  rowSums(
    do.call(cbind, lapply(elev, function(x) gaussian_beam_profile(height, beam_range(distance, x, k = k, lat = lat, re = re, rp = rp),
        x,
        antenna = antenna, beam_angle = beam_angle, lat = lat, k = k, re = re,
        rp = rp
      )))
  ) / length(elev)
}

# helper function for beam_profile_overlap()
beam_profile_overlap_help <- function(vp, elev, distance, antenna = 0,
                                      zlim = c(0, 4000), steps = 500,
                                      quantity = "dens", normalize = TRUE,
                                      beam_angle = 1, k = 4 / 3, lat, re = 6378,
                                      rp = 6357) {
  # define altitude grid
  height <- seq(zlim[1], zlim[2], length.out = steps)
  # calculate altitudinal radiation pattern of all radar beams combined
  beamprof <- beam_profile(
    height = height, distance = distance, elev = elev,
    antenna = antenna, lat = lat, beam_angle =
      beam_angle, k = k, re = re, rp = rp
  )
  # normalize the distribution
  step <- (zlim[2] - zlim[1]) / (steps - 1)
  if (normalize) beamprof <- beamprof / sum(beamprof * step)
  # output as data.frame
  beamprof <- data.frame(height = height, radiation = beamprof)
  if('height' %in% names(vp$data))
  {
    heightOrg<-vp$data$height
  }else{
    heightOrg<-vp$data$HGHT
  }
  # linearly interpolate the density of the vertical profile at the same grid as beamprof above
  beamprof$vpr <- approxfun(
    heightOrg + vp$attributes$where$interval / 2,
    vp$data[[quantity]]
  )(height)
  # normalize the vertical profile density
  step <- (zlim[2] - zlim[1]) / (steps - 1)
  beamprof$vpr <- beamprof$vpr / sum(step * beamprof$vpr, na.rm = T)
  # calculate the Bhattacharyya coefficient of the density profile and radiation coverage pattern
  sum(step * sqrt(beamprof$radiation * beamprof$vpr), na.rm = T)
}

#' Calculate overlap between a vertical profile ('vp') of biological scatterers
#' and the vertical radiation profile emitted by the radar
#'
#' Calculates the distribution overlap between a vertical profile ('vp')
#' and the vertical radiation profile of a set of emitted radar beams
#' at various elevation angles as given by \link{beam_profile}.
#' @inheritParams beam_height
#' @inheritParams beam_width
#' @param vp a vertical profile of class vp
#' @param elev numeric vector. Beam elevation(s) in degrees.
#' @param distance the distance(s) from the radar along sea level (down range) for which to calculate the overlap in m.
#' @param zlim altitude range in meter, given as a numeric vector of length two.
#' @param noise_floor The system noise floor in dBZ. The total system noise expressed as the reflectivity factor
#'  it would represent at a distance \code{noise_floor_ref_range} from the radar. NOT YET IMPLEMENTED
#' @param noise_floor_ref_range the reference distance from the radar at which \code{noise_floor} is expressed. NOT YET IMPLEMENTED
#' @param steps number of integration steps over altitude range zlim, defining altitude grid size used for numeric integrations
#' @param quantity profile quantity to use for the altitude distribution, one of 'dens' or 'eta'.
#' @param normalize Whether to normalize the radiation coverage pattern over the altitude range specified by zlim
#' @param antenna radar antenna height. If missing taken from \code{vp}
#' @param lat radar latitude. If missing taken from \code{vp}
#' @return A data.frame with columns distance and overlap.
#'
#' @export
#'
#' @details  Overlap is calculated as the  [Bhattacharyya coefficient](https://en.wikipedia.org/wiki/Bhattacharyya_distance)
#' (i.e. distribution overlap) between the (normalized) vertical profile vp and the (normalized)
#' radiation coverage pattern as calculated by \link{beam_profile}.
#'
#' The current implementation does not (yet) take into account the system noise floor when calculating
#' the overlap.
#'
#' In the ODIM data model the attribute \code{/how/NEZ} or \code{/how/NEZH} specifies the system noise floor
#' (the Noise Equivalent Z or noise equivalent reflectivity factor. the H refers to the horizontal channel
#' of a dual-polarization radar).
#' In addition, the attribute \code{/how/LOG} gives "security distance above mean noise level (dB) threshold value".
#' This is equivalent to the log receiver signal-to-noise ratio, i.e. the dB above the noise floor for the signal processor
#' to report a valid reflectivity value. We recommend using \code{NEZH}+\code{LOG} for \code{noise_floor}, as this
#' is the effective noise floor of the system below which no data will be reported by the radar signal processor.
#'
#' Typical values are \code{NEZH} = -45 to -50 dBZ at 1 km from the radar. \code{LOG} is typically around 1 dB.
#'
#' Need to evaluate beam by beam the returned signal relative to a uniform beam filling of at least NEZH + LOG
#' If returned signal is lower, the gate is below noise level.
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
#' bpo <- beam_profile_overlap(example_vp, get_elevation_angles(pvol), seq(0, 100000, 1000))
#' # plot the calculated overlap:
#' plot(bpo)
beam_profile_overlap <- function(vp, elev, distance, antenna, zlim = c(0, 4000),
                                 noise_floor = -Inf, noise_floor_ref_range = 1,
                                 steps = 500, quantity = "dens", normalize = T,
                                 beam_angle = 1, k = 4 / 3, lat, re = 6378,
                                 rp = 6357) {
  # min_detectable_eta <- dbz_to_eta(NEZH,vp$attributes$how$wavelength)*(range/1000)^2

  if (!is.numeric(elev)) stop("'elev' should be a number or numeric vector with the beam elevation(s)")
  if (!is.vp(vp)) stop("'vp' should be an object of class vp")
  if (missing(antenna)) antenna <- vp$attributes$where$height
  if (missing(lat)) lat <- vp$attributes$where$lat
  if (!is.numeric(distance) | min(distance) < 0) stop("'distance' should be a positive numeric value or vector")
  if (length(zlim) != 2 & !is.numeric(zlim)) stop("'zlim' should be a numeric vector of length two")
  if (is.na(zlim[1]) | is.na(zlim[2]) | zlim[1] > zlim[2]) stop("'zlim' should be a vector with two numeric values for upper and lower bound")
  if (length(steps) != 1 & !is.numeric(steps)) stop("'step' should be a numeric value")
  if (!(quantity %in% c("dens", "eta"))) stop("'quantity' should be one of 'dens' or 'eta'")
  if (is.null(vp$attributes$where$height) && missing(antenna)) stop("antenna height cannot be found in polar volume, specify antenna height using 'antenna' argument")
  assert_that(is.number(antenna))
  if (is.null(vp$attributes$where$lat) && missing(lat)) stop("radar latitude cannot be found in polar volume, specify using 'lat' argument")
  assert_that(is.number(lat))
  overlap <- sapply(distance, function(x) beam_profile_overlap_help(vp = vp, elev = elev, distance = x, antenna = antenna, zlim = zlim, steps = steps, quantity = quantity, normalize = normalize, beam_angle = beam_angle, k = k, lat = lat, re = re, rp = rp))
  data.frame(distance = distance, overlap = overlap)
}

#' Calculate radar beam distance
#'
#' Calculates the distance as measured over the earth's surface (the down range)
#' for a given beam elevation and slant range.
#'
#' @inheritParams beam_height
#' @return numeric. Beam distance (down range) in m.
#'
#' @export
#'
#' @details depends on \link{beam_height} to calculate beam height.
beam_distance <- function(range, elev, k = 4 / 3, lat = 35, re = 6378, rp = 6357) {
  er <- earth_radius(re, rp, lat)
  bh <- beam_height(range = range, elev = elev, k = k, lat = lat, re = re, rp = rp)
  er * acos((er^2 + (er + bh)^2 - range^2) / (2 * er * (er + bh)))
}

#' Calculate radar beam range
#'
#' Calculates the range (i.e. slant range) given a distance measured along the earth's surface
#' (i.e. down range) and beam elevation.
#'
#' @inheritParams beam_height
#' @param distance numeric. Distance from the radar as measured along sea level (down range) in m.
#' @return numeric. Beam range (slant range) in m.
#'
#' @export
#'
#' @details depends on \link{beam_height} to calculate beam height.
beam_range <- function(distance, elev, k = 4 / 3, lat = 35, re = 6378, rp = 6357) {
  er <- earth_radius(re, rp, lat)
  # to do: simplify trigonometry below
  -((2 * er * k * (k + cos(distance / er)) * (sin(distance / (2. * er))^2) * sin((elev * pi) / 180.) +
    sqrt((er^2) * (-1 + cos(distance / er)) * ((-1 + k + cos(distance / er))^2) *
      (-1 - 2 * (-1 + k) * k + (1 - 2 * k) * cos(distance / er) +
        k^2 * (1 + cos(distance / er)) * sin((elev * pi) / 180.)^2)))) /
    (-((-1 + k + cos(distance / er))^2) + (k^2) * (sin((elev * pi) / 180.)^2))
}
