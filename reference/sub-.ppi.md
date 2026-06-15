# Subset a plan position indicator (`ppi`)

Select parameters (`param`) or derived quantities by index from a plan
position indicator (`ppi`).

## Usage

``` r
# S3 method for class 'ppi'
x[i]
```

## Arguments

- x:

  A `ppi` object.

- i:

  Integer. Index/indices specifying which parameters (`param`) or
  derived quantities to extract.

## Value

A `ppi` object containing a subset of parameters (`param`).

## Examples

``` r
# Project a scan as a ppi
ppi <- project_as_ppi(example_scan)

# This ppi contains 5 parameters (DBZH VRADH ZDR RHOHV PHIDP)
ppi
#>                Plan position indicator (class ppi)
#> 
#>   parameters:  DBZH VRADH RHOHV ZDR PHIDP 
#>         dims:  201 x 201 pixels
#> 

# Subset ppi to one containing only the first parameter (DBZH)
ppi[1]
#>                Plan position indicator (class ppi)
#> 
#>   parameters:  DBZH 
#>         dims:  201 x 201 pixels
#> 

# Subset ppi to one containing the first three parameters (DBZH, VRADH, ZDR)
ppi[1:3]
#>                Plan position indicator (class ppi)
#> 
#>   parameters:  DBZH VRADH RHOHV 
#>         dims:  201 x 201 pixels
#> 

# Subset ppi to one without the first 2 parameters (ZDR RHOHV PHIDP)
ppi[-1:-2]
#>                Plan position indicator (class ppi)
#> 
#>   parameters:  RHOHV ZDR PHIDP 
#>         dims:  201 x 201 pixels
#> 
```
