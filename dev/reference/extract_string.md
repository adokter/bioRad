# extract strings from a vector using regex, analog to stringr::str_extract

extract strings from a vector using regex, analog to
stringr::str_extract

## Usage

``` r
extract_string(string, pattern, ...)
```

## Arguments

- string:

  Input vector. A character vector.

- pattern:

  Regex pattern to look for

- ...:

  passed on to [`regexpr()`](https://rdrr.io/r/base/grep.html)

## Value

A character vector with matches only, possibly of different length as
`string`
