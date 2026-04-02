# Read a polar volume (`pvol`) from file

Read a polar volume (`pvol`) from file

## Usage

``` r
read_pvolfile(
  file,
  param = c("DBZH", "DBZ", "VRADH", "VRAD", "WRADH", "WRAD", "TH", "T", "RHOHV", "ZDR",
    "PHIDP", "CELL", "BIOLOGY", "WEATHER", "BACKGROUND"),
  sort = TRUE,
  lat,
  lon,
  height,
  elev_min = 0,
  elev_max = 90,
  verbose = TRUE,
  mount = dirname(file),
  local_install
)
```

## Arguments

- file:

  A string containing the path to a polar volume file

- param:

  An atomic vector of character strings, containing the names of scan
  parameters to read. To read all scan parameters use 'all'.

- sort:

  A logical value, when `TRUE` sort scans ascending by elevation.

- lat:

  Latitude in decimal degrees of the radar position. If not specified,
  value stored in file is used. If specified, value stored in file is
  overwritten.

- lon:

  Longitude in decimal degrees of the radar position. If not specified,
  value stored in file is used. If specified, value stored in file is
  overwritten.

- height:

  Height of the center of the antenna in meters above sea level. If not
  specified, value stored in file is used. If specified, value stored in
  file is overwritten.

- elev_min:

  Minimum scan elevation to read in degrees.

- elev_max:

  Maximum scan elevation to read in degrees.

- verbose:

  A logical value, whether to print messages (`TRUE`) to console.

- mount:

  (deprecated) A character string with the mount point (a directory
  path) for the Docker container.

- local_install:

  (deprecated) String with path to local vol2bird installation, to use
  local installation instead of Docker container

## Value

An object of class
[pvol](http://adriaandokter.com/bioRad/dev/reference/summary.pvol.md),
which is a list containing polar scans, i.e. objects of class `scan`

## Details

Scan parameters are named according to the OPERA data information model
(ODIM), see Table 16 in the [ODIM
specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf).
Commonly available parameters are:

- `DBZH`, `DBZ`: (Logged) reflectivity factor (dBZ)

- `TH`, `T`: (Logged) uncorrected reflectivity factor (dBZ)

- `VRADH`, `VRAD`: Radial velocity (m/s). Radial velocities towards the
  radar are negative, while radial velocities away from the radar are
  positive

- `RHOHV`: Correlation coefficient (unitless). Correlation between
  vertically polarized and horizontally polarized reflectivity factor

- `PHIDP`: Differential phase (degrees)

- `ZDR`: (Logged) differential reflectivity (dB)

## Examples

``` r
# locate example volume file:
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")

# print the local path of the volume file:
pvolfile
#> [1] "/home/runner/work/_temp/Library/bioRad/extdata/volume.h5"

# load the file:
example_pvol <- read_pvolfile(pvolfile)

# print summary info for the loaded polar volume:
example_pvol
#>                Polar volume (class pvol)
#> 
#>      # scans:  3 
#>        radar:  seang 
#>       source:  WMO:02606,RAD:SE50,PLC:Angelholm,NOD:seang,ORG:82,CTY:643,CMT:Swedish radar 
#> nominal time:  2015-10-18 18:00:00 
#> 

# print summary info for the scans in the polar volume:
example_pvol$scans
#> [[1]]
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  0.5 deg
#>            dims:  480 bins x 360 rays
#> 
#> [[2]]
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  1.5 deg
#>            dims:  480 bins x 360 rays
#> 
#> [[3]]
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  2.5 deg
#>            dims:  480 bins x 360 rays
#> 

# copy the first scan to a new object 'scan'
scan <- example_pvol$scans[[1]]

# print summary info for the new object:
scan
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  0.5 deg
#>            dims:  480 bins x 360 rays
```
