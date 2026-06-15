# Convert a dataframe into a vpts object

Convert a dataframe into a vpts object

## Usage

``` r
as.vpts(data)
```

## Arguments

- data:

  a dataframe created from a VPTS CSV file

## Value

a bioRad vpts object

## Examples

``` r
# locate example file in VPTS CSV format:
df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))
# convert the data.frame to a vpts object:
as.vpts(df)
#> Warning: Validation issues found: Type validation failed for datetime; Type validation failed for vcp
#>                    Regular time series of vertical profiles (class vpts)
#> 
#>            radar:  bewid 
#>       # profiles:  49 
#> time range (UTC):  2023-05-03 18:00:00 - 2023-05-04 06:00:00 
#>    time step (s):  900 
```
