# Calculate Nyquist velocity for a given pulse repetition frequency (PRF)

Calculates the Nyquist velocity given a radar's pulse repetition
frequency (PRF) and wavelength. When specifying two PRFs, the extended
Nyquist velocity is given for a radar using the dual-PRF technique.

## Usage

``` r
nyquist_velocity(wavelength, prf1, prf2)
```

## Arguments

- wavelength:

  Numeric. Radar wavelength, in cm.

- prf1:

  Numeric. Radar pulse repetition frequency, in Hz.

- prf2:

  Numeric. Alternate radar pulse repetition frequency for a radar
  operating in dual-PRF mode, in Hz.

## Value

Nyquist velocity, in m/s.

## See also

Other conversion functions:
[`convert_legacy()`](http://adriaandokter.com/bioRad/dev/reference/convert_legacy.md),
[`dbz_to_eta()`](http://adriaandokter.com/bioRad/dev/reference/dbz_to_eta.md),
[`eta_to_dbz()`](http://adriaandokter.com/bioRad/dev/reference/eta_to_dbz.md)

## Examples

``` r
# Get Nyquist velocity at C-band (5.3 cm wavelength) and a PRF of 2000 Hz
nyquist_velocity(5.3, 2000)
#> [1] 26.5

# Get extended Nyquist velocity in a dual-PRF scheme using 2000 Hz and
# 1500 Hz PRFs
nyquist_velocity(5.3, 2000, 1500)
#> [1] 79.5
```
