## bioRad 0.5.2
This version includes CRAN-requested bugfix associated with the
R 4.0 release.

## Test environments
* local OS X install, R 3.6.3
* local Ubuntu 18.04 LTS, R 3.6.3
* local Windows 10 Enterprise, R 3.6.3
* remote R-devel https://builder.r-hub.io.
* remote R-devel Windows https://win-builder.r-project.org/

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    data      1.6Mb
    doc       1.2Mb
    extdata   1.7Mb

The tarball is however below the required 5 Mb

## r-hub builder
There was one NOTE, however the listed URLs are correct and accessible:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Adriaan M. Dokter <amd427@cornell.edu>’

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1175/1520-0426(2004)021<1566:DODRVU>2.0.CO;2
    From: man/calculate_vp.Rd
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: certificate has expired
      	(Status without verification: OK)
  URL: https://doi.org/10.1175/JTECH-D-17-0175.1
    From: man/calculate_param.Rd
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: certificate has expired
      	(Status without verification: OK)


## Downstream dependencies
There are currently no downstream dependencies for this package
