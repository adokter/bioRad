# A wrapper for [`sp::spTransform()`](https://edzer.github.io/sp/reference/spTransform.html). Converts geographic (WGS84) coordinates to a specified projection

A wrapper for
[`sp::spTransform()`](https://edzer.github.io/sp/reference/spTransform.html).
Converts geographic (WGS84) coordinates to a specified projection

## Usage

``` r
wgs_to_proj(lon, lat, proj4string)
```

## Arguments

- lon:

  Longitude

- lat:

  Latitude

- proj4string:

  An object of class 'CRS', as defined in package `sp`.

## Value

An object of class `SpatialPoints`.
