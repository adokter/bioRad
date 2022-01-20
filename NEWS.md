# bioRad 0.5.2.9XXX
* bugfix `local_mistnet` argument in `calculate_vp()` (#488)

* new optional argument `height_quantile` in `calculate_vp()` to calculate flight altitude quantiles (#485)

* bugfix for `max_interval` argument in `regularize_vpts()` (#480, #484)

* extension of `fill` argument in `regularize_vpts()` to allow specification of a time interval over which to perform nearest neighbor interpolation to fill gaps of missing profile data. (#475)

* altitude integration at altitude resolutions smaller than the altitude bin spacing. New option "antenna" for `alt_min` argument, setting the altitude of the antenna as the minimum altitude (#472)

* adding `write_pvolfile()` to write polar volumes to ODIM hdf5 format (#471)

* adding `attribute_table()` to quickly tabulate scan attributes (#365)

* `calculate_param()` now also works on ppi's (#316)

* Speed up `integrate_to_ppi` and other functions by avoiding duplicate input argument checking (#358)

* Warn when multiple scans with the same elevation are equally close to the requested elevation in `get_scan()`, and add option to return all (#414)

* `bind_into_vpts()` now works for vp's and vpts's with different heights (#343)

* bugfix ylim argument in `composite_ppi()` (#389)

* `scan_to_spatial()` now creates points for cell centers (#430)

* The package now account for `rstart` and `astart` from the odim specification. (#434)

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

* Improved documentation and unit tests for a number of functions thanks to an online bioRad sprint.

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

* `integrate_to_ppi()` is a new function to estimate spatial images of vertically integrated density and reflectivity. This function produces an `ppi` image showing the density of animals on the earth's surface, corrected for the changing overlap between the radar beams and animal layer with distance from the radar. See [Kranstauber et al. 2020](https://doi.org/10.3390/rs12040635) for methodology and [this vignette](https://adokter.github.io/bioRad/articles/range_correction.html) for examples.

* `apply_mistnet()` is a new function to apply the convolution neural network "MistNet" on pvolfiles to separate biological and meteorological signals (see [Lin et al. 2019](https://doi.org/10.1111/2041-210X.13280)). Results can be readily visualized with `plot.ppi()` and `plot.scan()`. MistNet is now also a segmentation option in `calculate_vp()` (#262).

* `read_pvolfile()` and `calculate_vp()` now read Vaisala IRIS RAW format directly, helpful for countries like 🇨🇦🇫🇮🇨🇴🇵🇹 (#222). bioRad now also reads files containing single elevation scans and `calculate_vp()` can calculate profiles from multiple files containing single elevation scans (#221).

### Newer/faster conversions

* `calculate_param()` is a new function to calculate parameters from existing parameters, e.g. reflectivity `eta` from reflectivity factor `DBZH` (#287).

* `scan_to_raster()` is a new function to convert a `scan` into a `RasterBrick` compatible with package [raster](https://cran.r-project.org/package=raster) (#238).

* `scan_to_spatial()` is a new function to convert a `scan` into a `SpatialPointsDataFrame` compatible with package [sp](https://cran.r-project.org/package=sp) (#238).

* `project_as_ppi()` is now much faster ([e420e5d](https://github.com/adokter/bioRad/commit/e420e5d)) and accounts for earth's curvature ([820e85f](https://github.com/adokter/bioRad/commit/820e85f)).

### New functions describing the radar beam geometry

* `beam_distance()` and `beam_range()` are new functions to relate range (i.e. slant range), distance (i.e. down range) and height of the radar beam.

* `beam_profile()` is a new function to calculate for a set of beam elevations the altitudinal normalized distribution of radiated energy by those beams.

* `beam_profile_overlap()` is a new function to calculate the distribution overlap (in terms of Bhattacharyya distance) between a vertical profile (`vp`) and the vertical radiation profile of a set of emitted radar beams (given by `beam_profile()`).

### Additional features

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

* [README](https://adokter.github.io/bioRad) reviewed for easier installation and usage (#155).

* [Get started vignette](https://adokter.github.io/bioRad/articles/bioRad.html) is now based on figure and workflow from [Dokter et al.](https://doi.org/10.1111/ecog.04028) (#168).

* [Introductory exercises vignette](https://adokter.github.io/bioRad/articles/rad_aero_19.html) renamed to `rad_aero_19.Rmd` since it is based on the 3d Radar Aeroecology Training School.

* bioRad now has a [code of conduct](https://adokter.github.io/bioRad/CODE_OF_CONDUCT.html) and [contributing guidelines](https://adokter.github.io/bioRad/CONTRIBUTING.html) (#145).

# bioRad 0.3.0

Release consistent with and in preparation of the bioRad methods paper (https://doi.org/10.1111/ecog.04028). All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/1?closed=1).

* Functions (#84), arguments (#112) and objects (#80) have been renamed to be consistent (#51). Deprecated functions will remain functional for now, but we will trigger a warning: **we advise to use the new functions names**. See the lists for [current functions](https://adokter.github.io/bioRad/reference/) and [deprecated functions](https://adokter.github.io/bioRad/reference/bioRad-deprecated.html).

* `integrate_profile()` replaces the functionality of `cmt()` (#75) and `mt()` (#76).

* `plot()` can now be used for scans (#71), e.g. `plot(example_scan)`.

* Functions are [organized in sections](https://adokter.github.io/bioRad/reference/) on the website (#110).

* Changelog section (this page) has been added to website (#144).

* Package R code is reorganized as one function = one file for easier maintenance (#50).

* First tests are included for some functions.

* Contributors (#90) and citation (#141) have been updated.

* bioRad now has a hex logo (#137). ✨
