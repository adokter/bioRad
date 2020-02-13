#' Convert reflectivity factor to reflectivity
#'
#' @param dbz reflectivity factor in dBZ
#' @param wavelength radar wavelength in cm
#' @param K norm of the complex refractive index of water
#'
#' @return reflectivity in cm^2/km^3
#'
#' @export
#' @examples
#' # calculate eta for a 7 dBZ reflectivity factor at C-band:
#' dbz_to_eta(7, 5)
#'
#' # calculate eta for a 7 dBZ reflectivity factor at S-band:
#' dbz_to_eta(7, 10)
#'
#' # calculate animal density for a 5 dBZ reflectivity
#' # factor at C-band and S-band, assuming a
#' # 11 cm^2 radar cross section per animal:
#' dbz_to_eta(7, 5) / 11 # C-band
#' dbz_to_eta(7, 10) / 11 # S-band
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
#' @examples
#' # reflectivity factor (dBZ) at C-band for a reflectivity eta=10000 cm^2/km^3:
#' eta_to_dbz(10000, 5)
#'
#' # reflectivity factor (dBZ) at S-band for a reflectivity eta=10000 cm^2/km^3:
#' eta_to_dbz(10000, 10)
#'
#' # expected reflectivity factor (dBZ) for an
#' # animal density of 1000 individuals/km^3
#' # and a radar cross section of 11 cm^2 per individual:
#' # at C-band and S-band:
#' eta_to_dbz(1000 * 11, 5) # C-band
#' eta_to_dbz(1000 * 11, 10) # S-band
eta_to_dbz <- function(eta, wavelength, K = 0.93) {
  10 * log10(eta * wavelength^4 / (1000 * (K^2) * pi^5))
}
