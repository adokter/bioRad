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

## comments after first reject by CRAN auto-check service
On first submission the system detected the following (possible) invalid URL:
https://doi.org/10.1111/ecog.04028
Ignoring as this link is available.

## Downstream dependencies
There are currently no downstream dependencies for this package
