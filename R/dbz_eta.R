#' Convert reflectivity factor to reflectivity
#'
#' @param dbz reflectivity factor in dBZ
#' @param wavelength radar wavelength in cm
#' @param K norm of the complex refractive index of water
#'
#' @return reflectivity in cm^2/km^3
#'
#' @export
dbz_to_eta <- function(dbz, wavelength, K = 0.93) {
  (1000 * pi^5 / wavelength^4) * (K^2) * (10^(dbz / 10))
}

#' Convert reflectivity to reflectivity factor
#'
#' @param eta reflectivity in cm^2/km^3
#' @param wavelength radar wavelength in cm
#' @param K norm of the complex refractive index of water
#'
#' @return reflectivity factor in dBZ
#'
#' @export
eta_to_dbz <- function(eta, wavelength, K = 0.93) {
  10 * log10(eta * wavelength^4 / (1000 * (K^2) * pi^5))
}
