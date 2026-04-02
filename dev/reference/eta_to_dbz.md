# Convert reflectivity (eta) to reflectivity factor (dBZ)

Converts reflectivity (eta) to reflectivity factor (dBZ).

## Usage

``` r
eta_to_dbz(eta, wavelength, K = sqrt(0.93))
```

## Arguments

- eta:

  Numeric. Reflectivity, in cm^2/km^3.

- wavelength:

  Numeric. Radar wavelength, in cm.

- K:

  Numeric. Norm of the complex refractive index of water.

## Value

Reflectivity factor, in dBZ.

## Examples

``` r
# Calculate dBZ for a 10000 cm^2/km^3 eta reflectivity at C-band
eta_to_dbz(10000, 5)
#> [1] 13.41648

# Calculate dBZ for a 10000 cm^2/km^3 eta reflectivity at S-band
eta_to_dbz(10000, 10)
#> [1] 25.45768

# Calculate dBZ for an animal density of 1000 individuals/km^3 and a radar
# cross section of 11 cm^2 per individual
eta_to_dbz(1000 * 11, 5) # C-band
#> [1] 13.8304
eta_to_dbz(1000 * 11, 10) # S-band
#> [1] 25.8716
```
