# Get elevation angles of a polar volume (`pvol`), scan (`scan`) or parameter (`param`)

Returns the elevation angles in degrees of all scans within a polar
volume (`pvol`) or the elevation angle of a single scan (`scan`) or scan
parameter (`param`).

## Usage

``` r
get_elevation_angles(x)

# S3 method for class 'pvol'
get_elevation_angles(x)

# S3 method for class 'scan'
get_elevation_angles(x)

# S3 method for class 'param'
get_elevation_angles(x)
```

## Arguments

- x:

  A `pvol`, `scan` or `param` object.

## Value

The elevation angle(s) in degrees.

## See also

- [`get_scan()`](http://adriaandokter.com/bioRad/reference/get_scan.md)

## Examples

``` r
# Locate and read the polar volume example file
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
example_pvol <- read_pvolfile(pvolfile)

# Get the elevations angles of the scans in the pvol
get_elevation_angles(example_pvol)
#> [1] 0.5 1.5 2.5

# Extract the first scan
scan <- example_pvol$scans[[1]]

# Get the elevation angle of that scan
get_elevation_angles(scan)
#> [1] 0.5
```
