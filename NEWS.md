# bioRad devel

* The plots for both `plot.vpts` and `plot.vp` got shifted up by half the height interval to reflect that `HGHT` refers to the bottom of the height interval


# bioRad 0.4.0

First release on CRAN! All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/4?closed=1).

* `get_param()` added as new function to get parameters from scan (#132).

* `download_basemap()` now uses Stamen basemaps by default (parameter `source`), so users do not need to request an API key for the previous default Google Maps (#163).

* `download_vpfiles()` and `select_vpfiles()` now use 5 letter radar codes (parameter `radars` instead of `radar` and `country`), allow to skip already downloaded files (parameter `overwrite = TRUE`) and have improved download and error messages (#176).

* `read_pvolfile()`, `nexrad_to_odim()` and `calculate_vp()` (all Docker dependent) can now read files from directories containing a space (#162).

* Details for deprecated functions `mt()`, `mtr()`, `cmt()` are now displayed with function (#166).

* [README](https://adokter.github.io/bioRad) reviewed for easier installation and usage (#155).

* [Get started vignette](https://adokter.github.io/bioRad/articles/bioRad.html) is now based on figure and workflow from [Dokter et al.](https://doi.org/10.1111/ecog.04028) (#168).

* [Introductory exercises vignette](https://adokter.github.io/bioRad/articles/rad_aero_18.html) renamed to `rad_aero_18.Rmd` since it is based on the 2nd Radar Aeroecology Training School.

* bioRad now has a [code of conduct](https://adokter.github.io/bioRad/CODE_OF_CONDUCT.html) and [contributing guidelines](https://adokter.github.io/bioRad/CONTRIBUTING.html) (#145).

# bioRad 0.3.0

Release consistent with and in preparation of the bioRad methods paper (https://doi.org/10.1111/ecog.04028). All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/1?closed=1).

* Functions (#84), arguments (#112) and objects (#80) have been renamed to be consistent (#51). Deprecated functions will remain functional for now, but we will trigger a warning: **we advise to use the new functions names**. See the lists for [current functions](../reference/) and [deprecated functions](../reference/bioRad-deprecated.html).

* `integrate_profile()` replaces the functionality of `cmt()` (#75) and `mt()` (#76).

* `plot()` can now be used for scans (#71), e.g. `plot(example_scan)`.

* Functions are [organized in sections](../reference/) on the website (#110).

* Changelog section (this page) has been added to website (#144).

* Package R code is reorganized as one function = one file for easier maintenance (#50).

* First tests are included for some functions.

* Contributors (#90) and citation (#141) have been updated.

* bioRad now has a hex logo (#137). ✨
