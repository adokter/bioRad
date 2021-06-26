#' Convert reflectivity factor (dBZ) to reflectivity (eta)
#'
#' Converts reflectivity factor (dBZ) to reflectivity (eta).
#'
#' @param dbz Numeric. Reflectivity factor, in dBZ.
#' @param wavelength Numeric. Radar wavelength, in cm.
#' @param K Numeric. Norm of the complex refractive index of water.
#'
#' @return Reflectivity, in cm^2/km^3.
#'
#' @export
#'
#' @seealso
#' * [eta_to_dbz()]
#'
#' @examples
#' # Calculate eta for a 7 dBZ reflectivity factor at C-band
#' dbz_to_eta(7, 5)
#'
#' # Calculate eta for a 7 dBZ reflectivity factor at S-band
#' dbz_to_eta(7, 10)
#'
#' # Calculate animal density for a 5 dBZ reflectivity factor, assuming a
#' # radar cross section of 11 cm^2 per individual
#' dbz_to_eta(7, 5) / 11 # C-band
#' dbz_to_eta(7, 10) / 11 # S-band
dbz_to_eta <- function(dbz, wavelength, K = 0.93) {
  assert_that(is.numeric(dbz))
  assert_that(
    is.numeric(wavelength) & all(wavelength > 0),
    msg = "`wavelength` must be a strictly positive numeric."
  )
  assert_that(
    is.numeric(K) & all(K > 0),
    msg = "`K` must be a strictly positive numeric."
  )
  (1000 * pi^5 / wavelength^4) * (K^2) * (10^(dbz / 10))
}

#' Convert reflectivity (eta) to reflectivity factor (dBZ)
#'
#' Converts reflectivity (eta) to reflectivity factor (dBZ).
#'
#' @param eta Numeric. Reflectivity, in cm^2/km^3.
#' @param wavelength Numeric. Radar wavelength, in cm.
#' @param K Numeric. Norm of the complex refractive index of water.
#'
#' @return Reflectivity factor, in dBZ.
#'
#' @export
#'
#' @seealso
#' * [dbz_to_eta()]
#'
#' @examples
#' # Calculate dBZ for a 10000 cm^2/km^3 eta reflectivity at C-band
#' eta_to_dbz(10000, 5)
#'
#' # Calculate dBZ for a 10000 cm^2/km^3 eta reflectivity at S-band
#' eta_to_dbz(10000, 10)
#'
#' # Calculate dBZ for an animal density of 1000 individuals/km^3 and a radar
#' # cross section of 11 cm^2 per individual
#' eta_to_dbz(1000 * 11, 5) # C-band
#' eta_to_dbz(1000 * 11, 10) # S-band
eta_to_dbz <- function(eta, wavelength, K = 0.93) {
  assert_that(
    is.numeric(eta) & all(eta > 0),
    msg = "`eta` must be a strictly positive numeric."
  )
  assert_that(
    is.numeric(wavelength) & all(wavelength > 0),
    msg = "`wavelength` must be a strictly positive numeric."
  )
  assert_that(
    is.numeric(K) & all(K > 0),
    msg = "`K` must be a strictly positive numeric."
  )
  10 * log10(eta * wavelength^4 / (1000 * (K^2) * pi^5))
}
