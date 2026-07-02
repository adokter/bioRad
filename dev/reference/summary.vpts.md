# Inspect a time series of vertical profiles (`vpts`)

R base functions for inspecting a time series of vertical profiles
(`vpts`) object.

## Usage

``` r
# S3 method for class 'vpts'
summary(object, ...)

# S3 method for class 'vpts'
print(x, digits = max(3L, getOption("digits") - 3L), ...)

is.vpts(x)

# S3 method for class 'vpts'
dim(x)
```

## Arguments

- object:

  A `vpts` object.

- ...:

  Additional arguments affecting the summary produced.

- x:

  A `vpts` object.

- digits:

  minimal number of *significant* digits, see
  [`print.default`](https://rdrr.io/r/base/print.default.html).

## Value

For `summary.vpts()`: prints the summary of the`vpts` object.

For `print.vpts()`: prints the summary of the `vpts` object.

For `is.vpts()`: `TRUE` for an object of class `vpts`, otherwise
`FALSE`.

For `dim.vpts()`: number of datetimes, heights and quantities in a time
series of vertical profiles (`vpts`).

## Details

A time series of vertical profiles contains time-ordered vertical
profiles (`vp)` of a single radar. This time series can be **regular**
(`vp` are equally spaced in time) or **irregular** (time steps between
`vp` are of unequal length), indicated in the field `regular`. Irregular
time series can be projected onto a regular time grid with
[`regularize_vpts()`](http://adriaandokter.com/bioRad/dev/reference/regularize_vpts.md).
A time series of vertical profile (`vp`) object is a list containing:

- `radar`: Radar identifier.

- `datetime`: Nominal times of the profiles (named `dates` in bioRad \<
  0.4.0) in UTC.

- `height`: Lowest height of the height bins in the profiles in m above
  sea level.

- `daterange`: Minimum and maximum nominal time of the profiles in UTC.

- `timesteps`: Time differences between the profiles. Element `i` gives
  the difference between profile `i` and `i+1`.

- `data`: A list of quantities, each containing a `datetime` by `height`
  matrix with the values. Use
  [`get_quantity()`](http://adriaandokter.com/bioRad/dev/reference/get_quantity.md)
  to access these and see
  [`summary.vp()`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)
  for a description of available quantities.

- `attributes`: List of the vertical profile's `what`, `where`, and
  `how` attributes, copied from the first profile.

- `regular`: Logical indicating whether the time series is regular or
  not.

## Conventions

- `NA`: Maps to `nodata` in the ODIM convention: value to denote areas
  void of data (never radiated).

- `NaN`: Maps to `undetect` in the ODIM convention: denote areas below
  the measurement detection threshold (radiated but nothing detected).
  The value is also used when there are too few datapoints to calculate
  a quantity.

- `0`: Maps to `0` in the ODIM convention: denote areas where the
  quantity has a measured value of zero (radiated and value zero
  detected or inferred).

## See also

Other vpts functions:
[`[.vpts()`](http://adriaandokter.com/bioRad/dev/reference/sub-.vpts.md),
[`example_vpts`](http://adriaandokter.com/bioRad/dev/reference/example_vpts.md),
[`plot.vpts()`](http://adriaandokter.com/bioRad/dev/reference/plot.vpts.md)

## Examples

``` r
# Check if an object is of class vpts
is.vpts(example_vpts)
#> [1] TRUE

# Get summary info
example_vpts # Same as summary(example_vpts) or print(example_vpts)
#>                    Irregular time series of vertical profiles (class vpts)
#> 
#>            radar:  KBGM 
#>       # profiles:  1934 
#> time range (UTC):  2016-09-01 00:02:00 - 2016-09-10 11:56:00 
#>    time step (s):  min: 180     max:  16320 

# Get dimensions
dim(example_vpts)
#> [1] 1934   25   15
```
