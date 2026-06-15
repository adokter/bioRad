# Deprecated bioRad functions and data

The functions and data listed below are deprecated or renamed and will
be defunct in the near future. When possible, alternative functions with
similar functionality are mentioned.

## Usage

``` r
check_docker(...)

update_docker(...)

vol2bird_version(...)

download_basemap(...)
```

## Value

`TRUE`

No return value, called for warning message side effect only

an object of class
[numeric_version](https://rdrr.io/r/base/numeric_version.html)

No return value, called for warning message side effect only

## check_docker

This function has been removed and always returns TRUE

## update_docker

This function has been deprecated

## vol2bird_version

This function has been moved to package vol2birdR

## download_basemap

This function has been deprecated ggmap has been replaced by ggspatial
which no longer requires a pre-downloaded raster basemap
