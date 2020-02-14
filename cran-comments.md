## Test environments
* local OS X install, R 3.6.2
* local Ubuntu 18.04 LTS, R 3.6.2
* local Windows 10 Enterprise, R 3.6.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    data      1.6Mb
    extdata   2.6Mb

We slightly exceed the recommended maximum package size of 5Mb
because of the inclusion of example radar data that are used
in various code examples. Weather radar data is notoriously 
large and we have already reduced the size of these files
(inst/extdata/volume.h5, data/example_scan.rda, data/example_vpts.rda)
as much as possible.

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

Additional NOTES:
Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
integrate_to_ppi 13.73   0.10   13.83
plot.scan         6.48   0.06    6.55

## Downstream dependencies
There are currently no downstream dependencies for this package
