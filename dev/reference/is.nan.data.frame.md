# Identify `NaN` in a dataframe

Identify cells with `NaN` (not a number) in a data frame. Improves on
the default[`base::is.nan()`](https://rdrr.io/r/base/is.finite.html)
function, which only works on vectors, by allowing data frames as input.

## Usage

``` r
# S3 method for class 'data.frame'
is.nan(x)
```

## Arguments

- x:

  A `data.frame` object.

## Value

A matrix of the same dimension as `x`, with `TRUE`/`FALSE` values for
whether each cell in the original data frame is a number or not.
