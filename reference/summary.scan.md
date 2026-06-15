# Inspect a scan (`scan`)

R base functions for inspecting a scan (`scan`) object.

## Usage

``` r
# S3 method for class 'scan'
summary(object, ...)

is.scan(x)

# S3 method for class 'scan'
dim(x)
```

## Arguments

- object:

  A `scan` object.

- ...:

  Additional arguments affecting the summary produced.

- x:

  A `scan` object.

## Value

For `summary.scan()`: prints a summary of the `scan` object

For `is.scan()`: `TRUE` for an object of class `scan`, otherwise
`FALSE`.

For `dim.scan()`: number of parameters (`param`), bins and rays in a
scan (`scan`).

## Details

A scan (or sweep) is made by the radar at a certain elevation angle. The
resulting parameter data (`param`) are organized along radar range
(bins) and azimuth (rays). A scan (`scan`) object is a list containing:

- `radar`: Radar identifier.

- `datetime`: Nominal time of the volume to which the scan belongs in
  UTC.

- `params`: List of scan parameters (`param`).

- `attributes`: List of the scan's `what`, `where` and `how` attributes.

- `geo`: List of the scan's geographic properties:

  - `lat`: Latitude of the radar in decimal degrees.

  - `lon`: Longitude of the radar in decimal degrees.

  - `height`: Height of the radar antenna in meters above sea level.

  - `elange`: Elevation angle of the radar beam for that scan in
    degrees.

  - `rscale`: Range bin size for that scan in m (e.g. 500 m \* 480 bins
    equals 240 km range).

  - `ascale`: Azimuth bin size for that scan in degrees (e.g. 1 degree
    \* 360 rays equals full circle).

  - `rstart`: The range where the first range gate starts in meters
    (note ODIM stores it as kilometers)

  - `astart`: The start of the first ray.

## See also

- [`get_scan()`](http://adriaandokter.com/bioRad/reference/get_scan.md)

- [`example_scan`](http://adriaandokter.com/bioRad/reference/example_scan.md)

- [`plot.scan()`](http://adriaandokter.com/bioRad/reference/plot.scan.md)

- [`get_param()`](http://adriaandokter.com/bioRad/reference/get_param.md)

## Examples

``` r
# Check if an object is of class scan
is.scan(example_scan)
#> [1] TRUE

# Get summary info
example_scan # Same as summary(example_scan) or print(example_scan)
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  0.5 deg
#>            dims:  480 bins x 360 rays

# Get dimensions
dim(example_scan)
#> [1]   5 480 360

# Get summary info for the parameters in the scan
example_scan$params
#> $DBZH
#>                Polar scan parameter (class param)
#> 
#>     quantity:  DBZH 
#>         dims:  480 bins x 360 rays
#> 
#> $VRADH
#>                Polar scan parameter (class param)
#> 
#>     quantity:  VRADH 
#>         dims:  480 bins x 360 rays
#> 
#> $RHOHV
#>                Polar scan parameter (class param)
#> 
#>     quantity:  RHOHV 
#>         dims:  480 bins x 360 rays
#> 
#> $ZDR
#>                Polar scan parameter (class param)
#> 
#>     quantity:  ZDR 
#>         dims:  480 bins x 360 rays
#> 
#> $PHIDP
#>                Polar scan parameter (class param)
#> 
#>     quantity:  PHIDP 
#>         dims:  480 bins x 360 rays
#> 
```
