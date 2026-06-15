# Skip test if no mistnet

Some functions require MistNet to be enabled in package vol2birdR. This
helper function allows to skip a test if MistNet is not available, e.g.
when running in CI. Inspired by
<https://testthat.r-lib.org/articles/skipping.html#helpers>.

## Usage

``` r
skip_if_no_mistnet()
```

## Value

Invisibly returns TRUE if MistNet is available, otherwise skips the test
with a message "No MistNet".
