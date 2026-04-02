# Adds expected eta to a scan

Adds expected eta to a scan

## Usage

``` r
add_expected_eta_to_scan(
  scan,
  vp,
  quantity = "dens",
  param = "DBZH",
  lat,
  lon,
  antenna,
  beam_angle = 1,
  k = 4/3,
  re = 6378,
  rp = 6357
)
```

## Arguments

- scan:

  a scan (sweep) of class scan

- vp:

  A `vp` object

- quantity:

  Character. Profile quantity on which to base range corrections, either
  `eta` or `dens`.

- param:

  reflectivity Character. Scan parameter on which to base range
  corrections. Typically the same parameter from which animal densities
  are estimated in `vp`. Either `DBZH`, `DBZV`, `DBZ`, `TH`, or `TV`.

- lat:

  Latitude of the radar, in degrees. If missing taken from `pvol`.

- lon:

  Latitude of the radar, in degrees. If missing taken from `pvol`.

- antenna:

  Numeric. Radar antenna height, in m. Default to antenna height in
  `vp`.

- beam_angle:

  Numeric. Beam opening angle in degrees, typically speciefied as the
  angle between the half-power (-3 dB) points of the main lobe for the
  one-way antenna pattern.

- k:

  Numeric. Standard refraction coefficient.

- re:

  Numeric. Earth equatorial radius, in km.

- rp:

  Numeric. Earth polar radius, in km.

## Value

A `scan` object.
