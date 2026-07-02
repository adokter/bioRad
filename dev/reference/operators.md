# Mathematical and arithmetic operations on param's, scan's and pvol's

Mathematical and arithmetic operations on param's, scan's and pvol's

## Usage

``` r
# S3 method for class 'scan'
Math(x, ...)

# S3 method for class 'pvol'
Math(x, ...)

# S3 method for class 'param'
Ops(e1, e2)

# S3 method for class 'scan'
Ops(e1, e2)

# S3 method for class 'pvol'
Ops(e1, e2)
```

## Arguments

- x:

  object of class `scan`, or `pvol`

- ...:

  objects passed on to the Math functions

- e1:

  object of class `param`, `scan`, `pvol` or a number

- e2:

  object of class `param`, `scan`, `pvol` or a number

## Value

an object of the input class

## Details

Use caution when applying these manipulations, as there are no
consistency checks if the operations lead to interpretable outcomes. For
example, when averaging scans with logarithmic values (e.g. DBZ), it
might be required to first exponentiate the data before summing.

Attributes are taken from the first object in the operation.

When a `pvol` is multiplied by a list, in which case arguments are taken
from the list per scan. this requires the list to have the same length
as the number of scans.

## Examples

``` r
# Locate and read the polar volume example file
scan1 <- example_scan

#add a value of 1 to all scan parameters:
scan2 <- example_scan + 1

# average the scan parameters of two scans:
# NB: requires identical scan parameter names and order!
(scan1 + scan2)/2
#>                   Polar scan (class scan)
#> 
#>      parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#> elevation angle:  0.5 deg
#>            dims:  480 bins x 360 rays
```
