#' Nyquist velocity for a given pulse repetition frequency (PRF)
#'
#' Calculates the Nyquist velocity given a radar's pulse repetition frequency (PRF)
#' and wavelength. When specifying two PRFs, the extended Nyquist
#' velocity is given for a radar using the dual-PRF technique.
#'
#' @param wavelength radar wavelength in cm
#' @param prf1 radar pulse repetition frequency in Hz
#' @param prf2 alternate radar pulse repetition frequency in Hz (for a radar opering in dual-PRF mode)
#'
#' @return Nyquist velocity in m/s.
#'
#' @export
#'
#' @examples
#' nyquist_velocity(5.3, 2000, 1500)
nyquist_velocity = function(wavelength, prf1, prf2){
  assert_that(is.number(wavelength))
  assert_that(is.number(prf1))
  if(missing(prf2)){
    return((wavelength / 100) * prf1 / 4)
  }
  else{
    assert_that(is.number(prf2))
    return(((wavelength / 100) / 4) * prf1*prf2 / abs(prf1 - prf2))
  }
}
