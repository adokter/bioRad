# Skip test if missing dependencies for mapping

Function map depends on several spatial dependencies (ggspatial,
prettymapr, rosm). This helper function allows to skip a test if these
dependencies are not available Inspired by
<https://testthat.r-lib.org/articles/skipping.html#helpers>.

## Usage

``` r
skip_if_no_mapping()
```

## Value

Invisibly returns TRUE if dependencies available, otherwise skips the
test with a message "map() dependencies (ggspatial, prettymapr, rosm)
not installed".
