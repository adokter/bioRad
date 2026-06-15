# convert a polar scan into a raster

convert an object of class 'scan' into a raster of class 'RasterBrick'

## Usage

``` r
scan_to_raster(
  scan,
  nx = 100,
  ny = 100,
  xlim,
  ylim,
  res = NA,
  param,
  raster = NA,
  lat,
  lon,
  crs = NA,
  k = 4/3,
  re = 6378,
  rp = 6357
)
```

## Arguments

- scan:

  a scan (sweep) of class scan

- nx:

  number of raster pixels in the x (longitude) dimension

- ny:

  number of raster pixels in the y (latitude) dimension

- xlim:

  x (longitude) range

- ylim:

  y (latitude) range

- res:

  numeric vector of length 1 or 2 to set the resolution of the raster
  (see
  [res](https://rspatial.github.io/terra/reference/dimensions.html)). If
  this argument is used, arguments `nx` and `ny` are ignored. Unit is
  identical to `xlim` and `ylim`.

- param:

  scan parameters to include. If `NA` include all scan parameters.
  Reducing the number of scan parameters speeds up evaluation.

- raster:

  (optional) `raster::RasterLayer` or
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with a CRS. When specified this raster topology is used for the
  output, and nx, ny, res arguments are ignored.

- lat:

  Geodetic latitude of the radar in degrees. If missing taken from
  `scan`.

- lon:

  Geodetic longitude of the radar in degrees. If missing taken from
  `scan`.

- crs:

  character or object of class CRS. PROJ.4 type description of a
  Coordinate Reference System (map projection). When 'NA' (default), an
  azimuthal equidistant projection with origin at the radar location is
  used. To use a WSG84 (lat,lon) projection, use crs="+proj=longlat
  +datum=WGS84"

- k:

  Numeric. Standard refraction coefficient.

- re:

  Numeric. Earth equatorial radius, in km.

- rp:

  Numeric. Earth polar radius, in km.

## Value

a RasterBrick

## Details

uses
[scan_to_spatial](http://adriaandokter.com/bioRad/reference/scan_to_spatial.md)
to georeference the scan's pixels. If multiple scan pixels fall within
the same raster pixel, the last added pixel is given (see
[rasterize](https://rspatial.github.io/terra/reference/rasterize.html)
for details).

## Examples

``` r
# \donttest{
# default projects full extent on 100x100 pixel raster:
scan_to_raster(example_scan)
#> class      : RasterBrick 
#> dimensions : 100, 100, 10000, 5  (nrow, ncol, ncell, nlayers)
#> resolution : 4791.787, 4791.787  (x, y)
#> extent     : -239589.3, 239589.3, -239589.3, 239589.3  (xmin, xmax, ymin, ymax)
#> crs        : +proj=aeqd +lat_0=56.3675003051758 +lon_0=12.8516998291016 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
#> source     : memory
#> names      :       DBZH,      VRADH,      RHOHV,        ZDR,      PHIDP 
#> min values :  -12.00000,  -21.43643,    0.02680,  -14.87059, -178.58824 
#> max values :  32.000000,  20.684274,   0.999300,   7.905883, 178.588244 
#> 

# crop the scan and project at a resolution of 0.1 degree:
scan_to_raster(example_scan, ylim = c(55, 57), xlim = c(12, 13), res = .1)
#> class      : RasterBrick 
#> dimensions : 20, 10, 200, 5  (nrow, ncol, ncell, nlayers)
#> resolution : 0.1, 0.1  (x, y)
#> extent     : 12, 13, 55, 57  (xmin, xmax, ymin, ymax)
#> crs        : +proj=aeqd +lat_0=56.3675003051758 +lon_0=12.8516998291016 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
#> source     : memory
#> names      :         DBZH,        VRADH,        RHOHV,          ZDR,        PHIDP 
#> min values :           NA,   -0.1880389,    0.9254000,   -0.7529409, -103.7647040 
#> max values :           NA,    0.3760777,    0.9951000,    0.6588239,  -27.5294080 
#> 

# using a template raster (a terra SpatRaster can be passed directly):
template_raster <- terra::rast(terra::ext(12, 13, 56, 58), crs = "epsg:4326")
scan_to_raster(example_scan, raster = template_raster)
#> class      : RasterBrick 
#> dimensions : 10, 10, 100, 5  (nrow, ncol, ncell, nlayers)
#> resolution : 0.1, 0.2  (x, y)
#> extent     : 12, 13, 56, 58  (xmin, xmax, ymin, ymax)
#> crs        : +proj=longlat +datum=WGS84 +no_defs 
#> source     : memory
#> names      :        DBZH,       VRADH,       RHOHV,         ZDR,       PHIDP 
#> min values :   -8.000000,  -20.684274,    0.078400,   -9.882353, -161.647058 
#> max values :   18.000000,   14.478992,    0.952100,    7.905883,  147.529420 
#> 
# }
```
