# Skip test if vol2birdR not installed

Some functions require suggested package vol2birdR to be installed. This
helper function allows to skip a test if vol2birdR is not available,
e.g. when running in CI. Inspired by
<https://testthat.r-lib.org/articles/skipping.html#helpers>.

## Usage

``` r
skip_if_no_vol2birdR()
```

## Value

Invisibly returns TRUE if vol2birdR is installed, otherwise skips the
test with a message "Package vol2birdR not installed".
