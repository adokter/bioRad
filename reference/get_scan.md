# Get a scan (`scan`) from a polar volume (`pvol`)

Returns the scan (`scan`) from a polar volume (`pvol`) with elevation
angle closest to `elev`.

## Usage

``` r
get_scan(x, elev, all = FALSE)
```

## Arguments

- x:

  A `pvol` object.

- elev:

  Numeric. Elevation angle in degrees.

- all:

  Logical. Return the first scan in the `pvol` object closest to the
  requested elevation (`FALSE`), or a list with all scans equally close
  to the requested elevation (`TRUE`).

## Value

A `scan` object when `all` equals `FALSE` (default), or a list of `scan`
objects if `all` equals `TRUE`

## Details

In cases where `elev` is exactly in between two scan elevation angles,
the lower elevation angle scan is returned.

## See also

- [`summary.scan()`](http://adriaandokter.com/bioRad/reference/summary.scan.md)

- [`get_elevation_angles()`](http://adriaandokter.com/bioRad/reference/get_elevation_angles.md)

## Examples

``` r
# Locate and read the polar volume example file
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

# Get elevation angles
get_elevation_angles(pvol)
#> [1] 0.5 1.5 2.5

# Extract the scan closest to 3 degrees elevation (2.5 degree scan)
scan <- get_scan(pvol, 3)

# Get summary info
scan
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  2.5 deg
#>            dims:  480 bins x 360 rays

# Extract all scans closest to 3 degrees elevation (2.5 degree scan)
# Always returns a list with scan object(s), containing multiple scans
# if the pvol contains multiple scans at the same closest elevation.
scan_list <- get_scan(pvol, 3)
scan_list
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  2.5 deg
#>            dims:  480 bins x 360 rays
```
