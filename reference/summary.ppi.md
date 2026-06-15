# Inspect a plan position indicator (`ppi`)

R base functions for inspecting a plan position indicator (`ppi`)
object.

## Usage

``` r
# S3 method for class 'ppi'
summary(object, ...)

is.ppi(x)

# S3 method for class 'ppi'
dim(x)
```

## Arguments

- object:

  A `ppi` object.

- ...:

  Additional arguments affecting the summary produced.

- x:

  A `ppi` object.

## Value

For `is.ppi()`: `TRUE` for an object of class `ppi`, otherwise `FALSE`.

For `dim.ppi()`: number of parameters (`param`), x and y pixels in a
plan position indicator (`ppi`).

## Details

A plan position indicator is a projection of radar data onto the earth's
surface, generated from a single scan (`scan`) with
[`project_as_ppi()`](http://adriaandokter.com/bioRad/reference/project_as_ppi.md),
a polar volume (`pvol`) with
[`integrate_to_ppi()`](http://adriaandokter.com/bioRad/reference/integrate_to_ppi.md)
or multiple plan position indicators (`ppi`) with
[`composite_ppi()`](http://adriaandokter.com/bioRad/reference/composite_ppi.md).
A plan position indicator (`ppi`) object is a list containing:

- `radar`: Radar identifier.

- `datetime`: Nominal time of the volume to which the scan belongs in
  UTC.

- `data`: A
  [`sp::SpatialGridDataFrame`](https://edzer.github.io/sp/reference/SpatialGridDataFrame.html)
  containing the georeferenced data. See
  [`summary.param()`](http://adriaandokter.com/bioRad/reference/summary.param.md)
  for commonly available parameters, such as `DBZH`.

- `geo`: List of the scan's geographic properties (see the `geo` element
  in
  [`summary.scan()`](http://adriaandokter.com/bioRad/reference/summary.scan.md)),
  with two additional properties:

  - `bbox`: Bounding box for the plan position indicator in decimal
    degrees.

  - `merged`: Logical. Flag to indicate if a plan position indicator is
    a composite of multiple scans. `TRUE` if generated with
    [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/reference/integrate_to_ppi.md)
    or
    [`composite_ppi()`](http://adriaandokter.com/bioRad/reference/composite_ppi.md).

## See also

- [`project_as_ppi()`](http://adriaandokter.com/bioRad/reference/project_as_ppi.md)

- [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/reference/integrate_to_ppi.md)

- [`plot.ppi()`](http://adriaandokter.com/bioRad/reference/plot.ppi.md)

- [`map()`](http://adriaandokter.com/bioRad/reference/map.md)

- [`composite_ppi()`](http://adriaandokter.com/bioRad/reference/composite_ppi.md)

- [`[ppi()`](http://adriaandokter.com/bioRad/reference/sub-.ppi.md)

## Examples

``` r
# Project a scan as a ppi
ppi <- project_as_ppi(example_scan)

# Check if it is an object of class ppi
is.ppi(ppi)
#> [1] TRUE

# Get summary info
ppi # Same as summary(ppi) or print(ppi)
#>                Plan position indicator (class ppi)
#> 
#>   parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#>         dims:  201 x 201 pixels
#> 

# Get dimensions
dim(ppi)
#> [1]   5 201 201
```
