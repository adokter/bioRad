# Bind vertical profiles (`vp`) into time series (`vpts`)

Used as helper function for the method dispatched `bind_into_vpts` and
keeping backward compatibility with the `vpts` function.

## Usage

``` r
vplist_to_vpts(x, radar = NA)
```

## Arguments

- x:

  A list of `vp` objects, usually a result of a call to
  [read_vpfiles](http://adriaandokter.com/bioRad/reference/read_vpfiles.md).

- radar:

  optional string containing the radar identifier to generate time
  series for.

## Value

an object of class
[vpts](http://adriaandokter.com/bioRad/reference/summary.vpts.md) when
`list` contains profiles of a single radar. A list of objects of class
[vpts](http://adriaandokter.com/bioRad/reference/summary.vpts.md) in
case when `list` contains profiles of multiple radars, containing
[vpts](http://adriaandokter.com/bioRad/reference/summary.vpts.md)
objects for each radar.

## Examples

``` r
vpfile1 <- system.file("extdata", "profile.h5", package = "bioRad")
vpfile2 <- vpfile1
vps <- read_vpfiles(c(vpfile1,vpfile2))
ts <- bind_into_vpts(vps)
```
