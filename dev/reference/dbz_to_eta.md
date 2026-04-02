# Convert reflectivity factor (dBZ) to reflectivity (eta)

Converts reflectivity factor (dBZ) to reflectivity (eta).

## Usage

``` r
dbz_to_eta(dbz, wavelength, K = sqrt(0.93))
```

## Arguments

- dbz:

  Numeric. Reflectivity factor, in dBZ.

- wavelength:

  Numeric. Radar wavelength, in cm.

- K:

  Numeric. Norm of the complex refractive index of water.

## Value

Reflectivity, in cm^2/km^3.

## See also

- [`eta_to_dbz()`](http://adriaandokter.com/bioRad/dev/reference/eta_to_dbz.md)

## Examples

``` r
# Calculate eta for a 7 dBZ reflectivity factor at C-band:
dbz_to_eta(7, 5)
#> [1] 2282.193

# Calculate eta for a 7 dBZ reflectivity factor at S-band:
dbz_to_eta(7, 10)
#> [1] 142.637

# Calculate animal density for a 5 dBZ reflectivity factor, assuming a
# radar cross section of 11 cm^2 per individual
dbz_to_eta(7, 5) / 11 # C-band
#> [1] 207.4721
dbz_to_eta(7, 10) / 11 # S-band
#> [1] 12.967
```
