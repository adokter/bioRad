# A wrapper for [`sp::spTransform()`](https://edzer.github.io/sp/reference/spTransform.html). Converts projected coordinates to geographic (WGS84) coordinates.

A wrapper for
[`sp::spTransform()`](https://edzer.github.io/sp/reference/spTransform.html).
Converts projected coordinates to geographic (WGS84) coordinates.

## Usage

``` r
proj_to_wgs(x, y, proj4string)
```

## Arguments

- x:

  The x-coordinate in the projected system.

- y:

  The y-coordinate in the projected system.

- proj4string:

  An object of class 'CRS', as defined in package `sp`.

## Value

An object of class `SpatialPoints`.
