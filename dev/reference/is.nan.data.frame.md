# Check for NaN values in data frames

S3 method for [`is.nan`](https://rdrr.io/r/base/is.finite.html) that
works with data frames. Identifies cells with `NaN` (not a number) in a
data frame, extending the base function which only works on vectors.

Identify cells with `NaN` (not a number) in a data frame. Improves on
the default[`base::is.nan()`](https://rdrr.io/r/base/is.finite.html)
function, which only works on vectors, by allowing data frames as input.

## Usage

``` r
# S3 method for class 'data.frame'
is.nan(x)

# S3 method for class 'data.frame'
is.nan(x)
```

## Arguments

- x:

  A `data.frame` object.

## Value

A logical matrix of the same dimensions as `x`, with `TRUE` for cells
containing `NaN` and `FALSE` otherwise

A matrix of the same dimension as `x`, with `TRUE`/`FALSE` values for
whether each cell in the original data frame is a number or not.

## Examples

``` r
df <- data.frame(
  a = c(1, 2, NaN),
  b = c(NaN, 5, 6)
)
is.nan(df)
#>          a     b
#> [1,] FALSE  TRUE
#> [2,] FALSE FALSE
#> [3,]  TRUE FALSE
```
