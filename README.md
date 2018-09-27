
<!-- README.md is generated from README.Rmd. Please edit that file -->
bioRad <img src="man/figures/logo.png" align="right">
=====================================================

bioRad aims to standardize methods for extracting and reporting biological signals from weather radars. It provides functionality to access low‐level radar data, process these data into meaningful biological information on animal speeds and directions at different altitudes in the atmosphere, visualize these biological extractions, and calculate further summary statistics.

To get started, see:

-   [Dokter et al. (2018)](https://doi.org/10.1111/ecog.04028): a paper describing the package.
-   [bioRad vignette](https://adokter.github.io/bioRad/articles/bioRad.html): an introduction to bioRad's main functionalities.
-   [Function reference](https://adokter.github.io/bioRad/reference/index.html): an overview of all bioRad functions.

Installation
------------

You can install the released version of bioRad from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bioRad") # Coming soon!
```

Alternatively, you can install the latest development version from [GitHub](https://github.com/adokter/bioRad) with:

``` r
devtools::install_github("adokter/bioRad")
```

Then load the package with:

``` r
library(bioRad)
#> Welcome to bioRad version 0.3.0
#> Docker daemon running, Docker functionality enabled.
```


``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!
