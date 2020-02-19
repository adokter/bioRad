## Test environments
* local OS X install, R 3.6.2
* local Ubuntu 18.04 LTS, R 3.6.2
* local Windows 10 Enterprise, R 3.6.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    data      1.6Mb
    doc       1.3Mb
    extdata   1.7Mb

The tarball is however below the required 5 Mb

## r-hub builder
Building bioRad with r-hub (using `check_rhub()` fails because gdal is
not available on the r-hub build system. The rgdal package is a
dependency of bioRad. rgdal documentations explains in detail
how to install gdal on all platforms, so this seems more an
rgdal issue than a bioRad issue.

Resulting PREPERROR:
```
#> configure: GDAL: 1.11.3
#> checking GDAL version >= 1.11.4... no
#> configure: error: upgrade GDAL to 1.11.4 or later
#> ERROR: configuration failed for package ‘rgdal’
#> * removing ‘/home/docker/R/rgdal’
```

## Downstream dependencies
There are currently no downstream dependencies for this package
