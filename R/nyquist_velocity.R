#' Calculate Nyquist velocity for a given pulse repetition frequency (PRF)
#'
#' Calculates the Nyquist velocity given a radar's pulse repetition frequency
#' (PRF) and wavelength. When specifying two PRFs, the extended Nyquist velocity
#' is given for a radar using the dual-PRF technique.
#'
#' @param wavelength Numeric. Radar wavelength, in cm.
#' @param prf1 Numeric. Radar pulse repetition frequency, in Hz.
#' @param prf2 Numeric. Alternate radar pulse repetition frequency for a radar
#'   operating in dual-PRF mode, in Hz.
#'
#' @return Nyquist velocity, in m/s.
#'
#' @export
#'
#' @examples
#' # Get Nyquist velocity at C-band (5.3 cm wavelength) and a PRF of 2000 Hz
#' nyquist_velocity(5.3, 2000)
#'
#' # Get extended Nyquist velocity in a dual-PRF scheme using 2000 Hz and
#' # 1500 Hz PRFs
#' nyquist_velocity(5.3, 2000, 1500)
nyquist_velocity <- function(wavelength, prf1, prf2) {
  assert_that(is.number(wavelength))
  assert_that(is.number(prf1))
  if (missing(prf2)) {
    return((wavelength / 100) * prf1 / 4)
  }
  else {
    assert_that(is.number(prf2))
    return(((wavelength / 100) / 4) * prf1 * prf2 / abs(prf1 - prf2))
  }
}
