## Test environments
* local OS X install, R 3.5.1
* local Ubuntu 16.04 LTS, R 3.4.4

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
* installing *source* package ‘rgdal’ ...
** package ‘rgdal’ successfully unpacked and MD5 sums checked
configure: R_HOME: /opt/R-devel/lib64/R
configure: CC: /usr/bin/clang
configure: CXX: /usr/bin/clang++
configure: C++11 support available
configure: rgdal: 1.3-6
checking for /usr/bin/svnversion... yes
configure: svn revision: 773
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘rgdal’
* removing ‘/home/docker/R/rgdal’
```

## Downstream dependencies
There are currently no downstream dependencies for this package
