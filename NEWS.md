
# bioRad 0.7.3.9000

** Bugfixes

* Updated the default refractive index value used in conversion of linear reflectivity (eta) to logarithmic reflectivity (dBZ) (#642). The effect is a 7% increase in animal densities in output of functions `integrate_to_ppi()` and `read_cajun()` only.

# bioRad 0.7.3

## New features

* Replaced the `ggmap` package with `ggspatial` for map visualizations. This change was made as `ggmap` no longer provides reliable open-source basemaps without the necessity to register for an API key (#638).

## Bugfixes

* Corrected incorrect mapping of to `dbz_all` data column in [VPTS CSV](https://aloftdata.eu/vpts-csv/) format to corresponding `DBZH` data column in bioRad vpts object (#634).

* Improved the polar volume downloads by `download_pvolfiles()`: now skips a day if there are issues with fetching instead of aborting the entire download (#636)

## Deprecations

* `download_basemap()` has been deprecated, function `map()` now automatically downloads a basemap (#638).

# bioRad 0.7.2

## Bugfixes

* changed default aloft bucket to aloftdata (#622)

* skip tests for `calculate_vp()` when vol2birdR package is not installed (#624)

* fix a bug in the calculation of flight altitude quantiles (#627), which caused underestimation of flight altitude quantiles by up to one altitude bin.

* updates for compatibility with testthat package 3rd edition ($630)

# bioRad 0.7.1

Rebuilds documentation with examples formatted as per CRAN requirements.

# bioRad 0.7.0

bioRad 0.7 includes a major backend overhaul that deprecates the use of Docker. All Docker-dependent functionality is now included in the new dependency package [vol2birdR](https://cran.r-project.org/package=vol2birdR) package, which needs to be installed as part of bioRad. All bioRad functions remain the same, but several functions will run considerable faster.

## New features

* bioRad is now fully available on Windows.

* Simplified installation, including automatic installation of rhdf5 from bioconductor (#464).

* Faster implementations of functions previously dependent on Docker, such as `calculate_vp()`, `apply_mistnet()` and `read_pvolfile()`.

* Support for reading [VPTS CSV](https://aloftdata.eu/vpts-csv/) format through updated function `read_vpts()`. VPTS CSV table schema included to allow offline parsing of VPTS CSV files as a [frictionless](https://CRAN.R-project.org/package=frictionless) data package (#551, #590).

* Updated function `read_vpts()` supports reading `vp`/`vpts` data in ODIM HDF and [VPTS CSV](https://aloftdata.eu/vpts-csv/) format (#551, #590).

* New function `list_vpts_aloft()` produces a list of [aloft](https://aloftdata.eu/browse/) archive URLs for time series of vertical profiles (`vpts`). This list of URLs can then be used to bulk download data using any number of external tools (#553).

* New function `read_stdout()` replaces previous functionality of `read_vpts()` to read vol2bird stdout format. It also has a new `sep` argument (#536) to support both fixed-delimited and comma-separated stdout data.

* New function `as.vpts` converts a data.frame originating from a VPTS CSV file into a vpts object (#555). Inverse operation of `as.data.frame.vpts()`.

* `read_pvolfiles()` now allows ODIM H5 files with missing `source` attribute. The functionality is similar to `read_vpfiles()`, i.e. extracting the NOD, RAD or WMO identifier, otherwise using `unknown` (2f6935c).

* `bind_into_vpts()` now works for vp and vpts objects with different heights (#343).

* Faster parallel mistnet runs (https://github.com/adokter/vol2birdR/issues/16).

## Bugfixes

* Fix bug in height integration, which excluded the lowest bins for certain low altitude radars (#534).

## Deprecations



* Argument `local_install` in `calculate_vp()` and `apply_mistnet()` is now deprecated.

* Functions `check_docker()` and `update_docker()` have been deprecated.

* Function `vol2bird_version()` has been migrated to package vol2birdR and can be accessed by `vol2birdR::vol2bird_version()`.

* Dependency `maptools` has been replaced with [suntools](https://github.com/adokter/suntools), `rgdal` has been removed in accordance with the evolution of `sp` and the [imminent archiving](https://r-spatial.org/r/2023/05/15/evolution4.html) of `rgdal`.

* Function `as.data.frame.vpts()` has output column names `lat`, `lon`, `antenna_height` renamed to `radar_latitude`, `radar_longitude`, `radar_height` for compatibility with the [VPTS CSV](https://aloftdata.eu/vpts-csv/) data format. The function also outputs an additional column `radar_wavelength` (#609).

# bioRad 0.6.1

Rebuilds the documentation using roxygen2 for compability with HTML5 (a CRAN requirement).

# bioRad 0.6.0

Introduces a number of new functions and parameters and includes bugfixes. All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/8?closed=1). 

## New functions

* New function `attribute_table()` to quickly tabulate scan attributes (#365).

* New function `get_iris_raw_task()` to returns `task` from IRIS files (#411).

* New functions `is.vpi()` and `summary()` for vpi objects (#380, #405).

* New function `write_pvolfile()` to write polar volumes to ODIM hdf5 format (#470, #471).

* New function `download_pvolfiles()` to download NEXRAD polar volume files from Amazon Web Services (#41, #487)

* New functions `Math.pvol()`, `Math.scan()`, `Ops.pvol()`,`Ops.scan()`,`Ops.param()`: standard mathematical operations for `param`, `scan` and `pvol` objects.

## New features and bugfixes

* `apply_mistnet()`, `nexrad_to_odim()` and `read_pvolfile()` can now use local vol2bird installation (#416, #398).

* `beam_profile_overlap()` and `integrate_to_ppi()` bugfix that affected profiles with only `NA` values (#396).

* `calculate_param()` now also works on PPIs (#316) + bugfix for lost attributes (#401) and long expressions without name (#399).

* `calculate_vp()` now sets default `range_max` to 35km (#206) + has a new optional parameter `height_quantile` to calculate flight altitude quantiles (#485) + parameters for local mistnet (#418, #488).

* `composite_ppi()` now composites multiple PPI parameters at once (#390, #393) + bugfix for `ylim` parameter (#389).

* `get_scan()` warns when multiple scans with the same elevation are equally close to the requested elevation and add option to return all (#414).

* `integrate_profile()` now sets default `interval_max` to 1h (#481) and has a new argument `interval_replace`. The function can now integrate at altitude resolutions smaller than the altitude bin spacing. New option `antenna` for `alt_min` parameter, setting the altitude of the antenna as the minimum altitude (#472). Directional angles are now always mapped to the 0-360 degree domain (#489).

* `integrate_to_ppi()` and other functions are sped up by avoiding duplicate input argument checking (#358) + radar name now added to output (#425, #443). Bugfix affecting profiles consisting of primarily NA/NaN values (#415), which are now treated as zeros.

* `map()` bugfix for transparency (#438) and base layer (#468).

* `plot.vpi()` now has an `elev` parameter (#412).

* `plot.vpts()` now supports custom color scales (#444). It can plot speed and direction as colours (#424) + height offset fixed in plots (#198) + bugfix negative `zlim` values (#402).

* `read_cajun()` bugfix for incorrect conversion of linear eta to reflectivity (#403).

* `read_vpts()` bugfix for missing height attribute (#409).

* `regularize_vpts()`'s `fill` parameter now allows to specify a time interval over which to perform nearest neighbour interpolation to fill gaps of missing profile data. (#475) + bugfix for `max_interval` parameter (#480, #484, #475).

* `scan_to_spatial()` now creates points for cell centers (#430).

## Additional features

* The package now accounts for `rstart` and `astart` from the ODIM specification (#434).

* Clearly indicate when a speed is ground speed (#462).

* Improved documentation and unit tests for a number of functions thanks to an online bioRad sprint (June 2021).

* GitHub Actions are set up to automatically test changes (#428).

# bioRad 0.5.2

This release is primarily a hotfix for R version 4.0 (#375). All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/11?closed=1). New features and improvements include:

* `regularize_vpts()` is now much faster, and chooses more intuitive starting and ending point of the regularized grid, e.g. projecting on half hour grid will have time series start on the nearest half hour (#332).

* `regularize_vpts()` has new option `keep_timestamp`, which allows individual profiles to keep there original timestamp instead of the timestamp of the regularized grid.

* `sunrise()`/`sunset()` have improved documentation (#180) and new option `force_tz` ([4968019](https://github.com/adokter/bioRad/commit/4968019)).

* `check_night()` has new option `offset`, which allows day/night transition to be shifted by a temporal offset (#338). For example, this is useful when selecting night time profiles that start a specific number of hours after sunset.

* `check_night()` now works for vpi objects ([23def64](https://github.com/adokter/bioRad/commit/23def64)).

* `filter_vpts()` allows to select for day and night in vpts using new arguments `night`, `elev` and `offset`, based on functionality of `check_night()` (#345).

* New functions `noy()` and `doy()` to determine which night or day of the year a profile belongs to (#346).

* `as.data.frame.vp()` now has separate function page and deprecated parameter `quantities` (#364).

* `get_quantity()` now has improved documentation and allows to return height (#352).

* `dim()` now returns dimensions in a different, more logical order for pvol and vpts objects (#355).

* Improved documentation and unit tests for a number of functions thanks to an online bioRad sprint (April 2020).

* Bugfixes (#330, #368).

# bioRad 0.5.1

Minor bugfixes. All issues included in this release can be found [here](https://github.com/adokter/bioRad/pull/334). This release primarily fixes a bug that will become effective once R version 4.0 is released.

* Fixes a conflict due to new raw data format introduced in R version 4.0 (#331).

* Corrects incorrect values in reading of correlation coefficient values RHOHV (#328).

* Bugfix in `read_cajun()` which introduced incorrect height column during refactoring in bioRad 0.5.0 release ([93ad0a4](https://github.com/adokter/bioRad/commit/93ad0a4)).

* Bugfix that fixes the mapping by `map()` of composites of composites (ppi's produced after repeated application of `composite_ppi()`, [a5c9048](https://github.com/adokter/bioRad/commit/a5c9048), [043aa73](https://github.com/adokter/bioRad/commit/043aa73)).

* Minor bug fixes, and addressing conflicts with CRAN dependencies.

* Extend the functionality of `composite_ppi()` and improve its documentation (partial fix of #59).

* Fix a bug in the color legend of `map()` when providing a custom color scale with palette argument (#324).

* Minor documentation improvements.

# bioRad 0.5.0

New CRAN release. All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/6?closed=1).

## New features

* `integrate_to_ppi()` is a new function to estimate spatial images of vertically integrated density and reflectivity. This function produces an `ppi` image showing the density of animals on the earth's surface, corrected for the changing overlap between the radar beams and animal layer with distance from the radar. See [Kranstauber et al. 2020](https://doi.org/10.3390/rs12040635) for methodology and [this vignette](https://adriaandokter.com/bioRad/articles/range_correction.html) for examples.

* `apply_mistnet()` is a new function to apply the convolution neural network "MistNet" on pvolfiles to separate biological and meteorological signals (see [Lin et al. 2019](https://doi.org/10.1111/2041-210X.13280)). Results can be readily visualized with `plot.ppi()` and `plot.scan()`. MistNet is now also a segmentation option in `calculate_vp()` (#262).

* `read_pvolfile()` and `calculate_vp()` now read Vaisala IRIS RAW format directly, helpful for countries like 🇨🇦🇫🇮🇨🇴🇵🇹 (#222). bioRad now also reads files containing single elevation scans and `calculate_vp()` can calculate profiles from multiple files containing single elevation scans (#221).

## Newer/faster conversions

* `calculate_param()` is a new function to calculate parameters from existing parameters, e.g. reflectivity `eta` from reflectivity factor `DBZH` (#287).

* `scan_to_raster()` is a new function to convert a `scan` into a `RasterBrick` compatible with package [raster](https://cran.r-project.org/package=raster) (#238).

* `scan_to_spatial()` is a new function to convert a `scan` into a `SpatialPointsDataFrame` compatible with package [sp](https://cran.r-project.org/package=sp) (#238).

* `project_as_ppi()` is now much faster ([e420e5d](https://github.com/adokter/bioRad/commit/e420e5d)) and accounts for earth's curvature ([820e85f](https://github.com/adokter/bioRad/commit/820e85f)).

## New functions describing the radar beam geometry

* `beam_distance()` and `beam_range()` are new functions to relate range (i.e. slant range), distance (i.e. down range) and height of the radar beam.

* `beam_profile()` is a new function to calculate for a set of beam elevations the altitudinal normalized distribution of radiated energy by those beams.

* `beam_profile_overlap()` is a new function to calculate the distribution overlap (in terms of Bhattacharyya distance) between a vertical profile (`vp`) and the vertical radiation profile of a set of emitted radar beams (given by `beam_profile()`).

## Additional features

* Custom color scales in `map()` and `plot.ppi` (#318).

* `nyquist_velocity()` is a new function to calculate the unambiguous velocity of Doppler radar from its pulse repetition frequency/frequencies (#208).

* `filter_vpts()` is a new function simplifying the selection of time ranges and instances in vertical profile time series (`vpts`) (#241).

* The definition of height-integrated velocity is now corrected (#232, #233, [72be6d1](https://github.com/adokter/bioRad/commit/72be6d1)).

*  Improved documentation of how mtr can be calculated from vid, u, v ([6dce625](https://github.com/adokter/bioRad/commit/6dce625)).
 
* `read_pvolfile()` now also reads quantities DBZ, TH, T, because these often occur in European data and are relevant for biological analysis (note that DBZ and T are not ODIM-compliant names) ([5db08bd](https://github.com/adokter/bioRad/commit/5db08bd)).

* `plot.scan()` now has correct ordering of rays (#285).

* `plot.vpts()` now has correctly positioned and rounded speed barbs (#277, #244).

* `integrate_profile()`'s default unit of interval argument has been changed to seconds (#234).

* `project_as_ppi()`'s default projection is now on earth's surface (#280).

* `height` is now the default quantity denoting height above mean sea level. `HGHT` is deprecated (#273).

* `plot.vpts()` and `plot.vp()` plots are shifted up by half the height interval to reflect that height refers to the bottom of the height interval (#277, #198).

* `calculate_vp()`'s default `sd_vvp_threshold` parameter value at S-band is now 1 m/s (#93).

* And many small bug fixes and documentation improvements.

# bioRad 0.4.0

First release on CRAN! All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/4?closed=1).

* `get_param()` added as new function to get parameters from scan (#132).

* `download_basemap()` now uses Stamen basemaps by default (parameter `source`), so users do not need to request an API key for the previous default Google Maps (#163).

* `download_vpfiles()` and `select_vpfiles()` now use 5 letter radar codes (parameter `radars` instead of `radar` and `country`), allow to skip already downloaded files (parameter `overwrite = TRUE`) and have improved download and error messages (#176).

* `read_pvolfile()`, `nexrad_to_odim()` and `calculate_vp()` (all Docker dependent) can now read files from directories containing a space (#162).

* Details for deprecated functions `mt()`, `mtr()`, `cmt()` are now displayed with function (#166).

* [README](https://adriaandokter.com/bioRad/) reviewed for easier installation and usage (#155).

* [Get started vignette](https://adriaandokter.com/bioRad/articles/bioRad.html) is now based on figure and workflow from [Dokter et al.](https://doi.org/10.1111/ecog.04028) (#168).

* [Introductory exercises vignette](https://adriaandokter.com/bioRad/articles/rad_aero_19.html) renamed to `rad_aero_19.Rmd` since it is based on the 3d Radar Aeroecology Training School.

* bioRad now has a [code of conduct](https://adriaandokter.com/bioRad/CODE_OF_CONDUCT.html) and [contributing guidelines](https://adriaandokter.com/bioRad/CONTRIBUTING.html) (#145).

# bioRad 0.3.0

Release consistent with and in preparation of the bioRad methods paper (https://doi.org/10.1111/ecog.04028). All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/1?closed=1).

* Functions (#84), arguments (#112) and objects (#80) have been renamed to be consistent (#51). Deprecated functions will remain functional for now, but we will trigger a warning: **we advise to use the new functions names**. See the lists for [current functions](https://adriaandokter.com/bioRad/reference/) and [deprecated functions](https://adriaandokter.com/bioRad/reference/bioRad-deprecated.html).

* `integrate_profile()` replaces the functionality of `cmt()` (#75) and `mt()` (#76).

* `plot()` can now be used for scans (#71), e.g. `plot(example_scan)`.

* Functions are [organized in sections](https://adriaandokter.com/bioRad/reference/) on the website (#110).

* Changelog section (this page) has been added to website (#144).

* Package R code is reorganized as one function = one file for easier maintenance (#50).

* First tests are included for some functions.

* Contributors (#90) and citation (#141) have been updated.

* bioRad now has a hex logo (#137). ✨
