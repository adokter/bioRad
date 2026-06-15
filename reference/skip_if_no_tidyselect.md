# Skip test if no tidyselect

dplyr select method require package tidyselect This helper function
allows to skip a test if tidyselect is not available Inspired by
<https://testthat.r-lib.org/articles/skipping.html#helpers>.

## Usage

``` r
skip_if_no_tidyselect()
```

## Value

Invisibly returns TRUE if tidyselect is available, otherwise skips the
test with a message "Package tidyselect not installed".
