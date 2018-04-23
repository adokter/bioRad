#' Calculate radar beam height
#'
#' Calculates the height of a radar beam as a function of elevation and range,
#' assuming the beam is emitted at surface level.
#'
#' @param range numeric. Range (distance from the radar antenna) in km.
#' @param elev numeric. Elevation in degrees.
#' @param k Standard refraction coefficient.
#' @param re Earth equatorial radius in km.
#' @param rp Earth polar radius in km.
#' @param lat Geodetic latitude in degrees.
#'
#' @return numeric. Beam height in km.
#'
#' @export
#'
#' @details To account for refraction of the beam towards the earth's surface,
#' an effective earth's radius of 4/3 * (true radius) is assumed.
#'
#' The earth's radius is approximated as a point on a spheroid surface, with
#' \code{re} the longer equatorial radius, and \code{rp} the shorter polar
#' radius. Typically uncertainties in refraction coefficient are relatively
#' large, making oblateness of the earth and the dependence of earth radius with
#' latitude only a small correction. Using default values assumes an average
#' earth's radius of 6371 km.
beam_height <- function(range, elev, k = 4/3, lat = 35, re = 6378, rp = 6357) {
  sqrt(range^2 + (k * earth_radius(re, rp, lat))^2 +
         2 * range * (k*earth_radius(re, rp, lat)) * sin(elev * pi/180)
       ) - k * earth_radius(re, rp, lat)
}

earth_radius <- function(a, b, latdeg) {
  lat <- latdeg * pi/180
  sqrt(((a^2*cos(lat))^2 + (b^2*sin(lat))^2)/((a*cos(lat))^2 + (b*sin(lat))^2))
}

#' Calculate radar beam width
#'
#' Calculates the width of a radar beam as a function of range and beam angle.
#'
#' @param range numeric. Range (distance from the radar antenna) in km.
#' @param angle numeric. Beam angle in degrees.
#' @return numeric. Beam width in m.
#'
#' @export
beam_width <- function(range, angle = 1) range*1000*sin(angle*pi/180)
