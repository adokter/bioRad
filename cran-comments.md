## bioRad 0.5.1
This version includes a minor bugfix associated with the
upcoming R 0.4.0 release. I only found out today that our
package had been removed from CRAN because of this, without
any notice, is there a way for me to subscribe to a notification
when my package is pending to be removed?

## Test environments
* local OS X install, R 3.6.3
* local Ubuntu 18.04 LTS, R 3.6.3
* local Windows 10 Enterprise, R 3.6.3

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
There was one warning:

* checking package dependencies ... WARNING
Requires orphaned package: 'ggmap'

## Downstream dependencies
There are currently no downstream dependencies for this package
