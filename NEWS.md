# bioRad 0.5.0

New CRAN release. All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/6?closed=1).

### Main new features:
* New method added for estimating spatial images of vertically integrated density and reflectivity (see [Kranstauber et al. 2020](https://doi.org), accessible through function `integrate_to_ppi()`. This function produces an image (as a `ppi` object) showing the density of animals on the earth's surface, corrected for the changing overlap between the radar beams and animal layer with distance from the radar. Read the paper and check out the [vignette](http://adriaandokter.com/bioRad/dev/articles/range_correction.html) for examples.

* Implementation of the convolution neural network MistNet for separating biological and meteorological signals in radar images (#262) (see [Lin et al. 2019](https://doi.org/10.1111/2041-210X.13280)). MistNet is now a segmentation option in function `calculate_vp()`. New function `apply_mistnet()` reads segmentation results, which can be readily visualized with `plot.ppi` and `plot.scan` functions.

* Support for more data formats. bioRad now reads Vaisala IRIS RAW format directly, helpful for countries like 🇨🇦 🇫🇮🇨🇴🇵🇹 (#222). bioRad also reads files containing single elevation scans, and profiles can be calculate using `calculate_vp` from multiple files containing single elevation scans (#221)

### New and faster conversion functions: 
* new function `calculate_param()` for calculating new scan parameters from existing scan parameters, e.g. linear reflectivity `eta` from reflectivity factor `DBZH` (#287)

* `scan_to_raster()` to convert a `scan` into a `RasterBrick` compatible with package raster (#238)

* `scan_to_spatial()` to convert a `scan` into a `SpatialPointsDataFrame` compatible with package sp (#238)

* `project_as_ppi()` function is now much faster (e420e5d), and accounts for earth's curvature (820e85f)

### New functions describing the radar beam geometry:
* new functions `beam_distance()`, `beam_range()` to relate range (i.e. slant range), distance (i.e. down range) and height of the radar beam.

* new function `beam_profile()` to calculate for a set of beam elevations the altitudinal normalized distribution of radiated energy by those beams.

* new function `beam_profile_overlap()` to calculate the distribution overlap (in terms of Bhattacharyya distance)  between a vertical profile (`vp`) and the vertical radiation profile of a set of emitted radar beams (given by `beam_profile()`)

### Additional features and changes:
* new function `nyquist_velocity()` to calculate the unambiguous velocity of Doppler radar from its pulse repetition frequency/frequencies (#208)

* new function `filter_vpts()`, simplifying the selection of time ranges and instances in vertical profile time series objects (`vpts`) (#241)

* corrected the definition of height-integrated velocity (issues #232, #233, 72be6d1)

* improved documentation of how mtr can be calculated from vid, u, v (6dce625)

* `read_pvolfile()` now also reads quantities DBZ, TH, T, because these often occur in European data and are relevant for biological analysis (note that DBZ and T are not ODIM-complient names) (5db08bd)

* fix in `plot.scan()`, correcting the ordering of rays (#285)

* fixes to position and rounding of speed barbs in `plot.vpts()` (#277, #244)

* default unit of interval argument in `integrate_profile()` changed to seconds (#234)

* default projection of `project_as_ppi()`is now onto earth's surface (#280)

* avoid mixing up of quantity and param arguments with informative message (#267)

* `height` is now the default quantity denoting height above mean sea level. `HGHT` is deprecated (#273)

* The plots for both `plot.vpts` and `plot.vp` got shifted up by half the height interval to reflect that `height` refers to the bottom of the height interval (#277, #198)

* in `calculate_vp()` the default `sd_vvp_threshold` parameter value at S-band is now 1 m/s (#93)

* and many small bug fixes and documentation improvements.

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
