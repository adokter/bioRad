# Changelog

## bioRad 0.12.0.9000

- [`apply_mistnet()`](http://adriaandokter.com/bioRad/dev/reference/apply_mistnet.md)
  now accepts `pvol` objects in addition to pvolfiles.

- bugfix
  [`calculate_param()`](http://adriaandokter.com/bioRad/dev/reference/calculate_param.md)
  when using `ifelse`
  ([\#770](https://github.com/adokter/bioRad/issues/770)).

- Improve axis labels for
  [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) and
  provide option not to plot the radar
  ([\#773](https://github.com/adokter/bioRad/issues/773)).

- bugfix
  [`integrate_profile()`](http://adriaandokter.com/bioRad/dev/reference/integrate_profile.md)
  for height-integrated speed quantities (`u`,`v`,`ff`) in vpts objects
  ([\#782](https://github.com/adokter/bioRad/issues/782)).

## bioRad 0.12.0

CRAN release: 2026-06-13

### New features

- [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  now supports profile estimation with height bins defined relative to
  ground level and antenna level
  ([\#686](https://github.com/adokter/bioRad/issues/686)).

- [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  now supports range correction with profiles referenced to ground
  level, accounting for local topography
  ([\#747](https://github.com/adokter/bioRad/issues/747),
  [\#748](https://github.com/adokter/bioRad/issues/748)).

- New function
  [`add_param()`](http://adriaandokter.com/bioRad/dev/reference/add_param.md)
  for mapping raster data onto `scan` and `pvol` object
  ([\#767](https://github.com/adokter/bioRad/issues/767)).

- New output field `eta_sum_to_VIR` in
  [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  ([\#748](https://github.com/adokter/bioRad/issues/748)).

- New additional arguments `alt_min` and `filter_all_heights` in
  [`filter_precip()`](http://adriaandokter.com/bioRad/dev/reference/filter_precip.md)
  for precip filtering at high altitudes only
  ([\#754](https://github.com/adokter/bioRad/issues/754),
  [\#755](https://github.com/adokter/bioRad/issues/755)).

- [`beam_width()`](http://adriaandokter.com/bioRad/dev/reference/beam_width.md)
  and related functions have a new argument `path` for outputting either
  the one-way or two-way beam width of the antenna pattern. The default
  is changed to two-way (formerly one-way)
  ([\#744](https://github.com/adokter/bioRad/issues/744)).

- Expose `eta_max` as a user option in
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md),
  to set maximum bird reflectivity (maps to option `etaMax` in
  [`vol2birdR::vol2bird_config()`](https://rdrr.io/pkg/vol2birdR/man/vol2bird_config.html))
  ([\#763](https://github.com/adokter/bioRad/issues/763))

- Expose `max_nyquist_dealias` parameter to set maximum Nyquist velocity
  above which to suppress dealiasing (maps to option `maxNyquistDealias`
  in
  [`vol2birdR::vol2bird_config()`](https://rdrr.io/pkg/vol2birdR/man/vol2bird_config.html))
  ([\#684](https://github.com/adokter/bioRad/issues/684)).

- `raster` argument of
  [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md),
  [`project_as_ppi()`](http://adriaandokter.com/bioRad/dev/reference/project_as_ppi.md)
  and `scan_convert` now accepts `terra:SpatRaster` class (8f8458a)

### Bugfixes

- Bugfix parsing `azim_min`, `azim_max` and `elev_max` arguments for
  pvol objects ([\#762](https://github.com/adokter/bioRad/issues/762)).

- Changed lower bound of allowed values for `nyquist_min` argument of
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  to zero ([\#761](https://github.com/adokter/bioRad/issues/761)).

- Bugfix for
  [`as.vpts()`](http://adriaandokter.com/bioRad/dev/reference/as.vpts.md)
  fixing uninformative error message in the case of NA values in the
  `source_file` column of a vpts data.frame
  ([\#759](https://github.com/adokter/bioRad/issues/759)).

- Bugfix for
  [`filter_precip()`](http://adriaandokter.com/bioRad/dev/reference/filter_precip.md)
  not being applied to `vp` objects
  ([\#755](https://github.com/adokter/bioRad/issues/755)).

- Bugfix correcting uninformative error message when integer value
  columns in vpts data.frame contain `NA` values
  ([\#755](https://github.com/adokter/bioRad/issues/755)).

- Bugfix correcting removals of pure insect cases in
  [`clean_mixture()`](http://adriaandokter.com/bioRad/dev/reference/clean_mixture.md)
  ([\#753](https://github.com/adokter/bioRad/issues/753)).

- [`eta_to_dbz()`](http://adriaandokter.com/bioRad/dev/reference/eta_to_dbz.md)
  now accepts NA or NaN input reflectivity values
  ([\#741](https://github.com/adokter/bioRad/issues/741)).

- Fix typo in documentation
  [`clean_mixture()`](http://adriaandokter.com/bioRad/dev/reference/clean_mixture.md)
  swapping birds and insects (bd7e89d).

## bioRad 0.11.0

CRAN release: 2025-08-29

### New features

- Faster
  [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  when using standard aeqd projection grid
  ([\#734](https://github.com/adokter/bioRad/issues/734)).

- Cross references to new `getRad` package for downloading EU and US
  polar volume data
  ([\#729](https://github.com/adokter/bioRad/issues/729)).

### Bugfixes

- Removed `aws.s3` as a dependency, functions depending on `aws.s3` now
  using `httr2`. New hidden helper functions in `utils.R` replacing
  `aws.s3` functionality
  ([\#731](https://github.com/adokter/bioRad/issues/731),
  [\#732](https://github.com/adokter/bioRad/issues/732)).

- Package now depends on R (\>= 4.1.0) for `httr2` compatibility.

- NEXRAD data is now downloaded from `unidata-nexrad-level2` bucket
  ([\#735](https://github.com/adokter/bioRad/issues/735)).

## bioRad 0.10.0

CRAN release: 2025-06-16

### New features

- New argument `directory_tree` in function
  [`download_pvolfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_pvolfiles.md)
  for omitting local directory tree
  ([\#710](https://github.com/adokter/bioRad/issues/710)).

- Use default Bootstrap 5 styling for pkgdown website.

- Functions
  [`download_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_vpfiles.md)
  and
  [`list_vpts_aloft()`](http://adriaandokter.com/bioRad/dev/reference/list_vpts_aloft.md)
  have been superseded in favour of
  [`getRad::get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.html)
  ([\#715](https://github.com/adokter/bioRad/issues/715)).

### Bugfixes

- Fix in
  [`project_as_ppi()`](http://adriaandokter.com/bioRad/dev/reference/project_as_ppi.md)
  for a bug introduced in bioRad 0.8.0 that produced incorrect
  projections of scans with a nonzero range or azimuth offsets. These
  offsets are stored as `astart` and `rstart` metadata in ODIM H5
  ([\#721](https://github.com/adokter/bioRad/issues/721)).

- Improve verbosity flags (`verbose`, `warnings`) in
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  for compatibility with new vol2birdR release
  ([\#718](https://github.com/adokter/bioRad/issues/718)).

- Improve error message of
  [`download_pvolfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_pvolfiles.md)
  when remote server is briefly not reachable
  ([\#726](https://github.com/adokter/bioRad/issues/726))

## bioRad 0.9.1

CRAN release: 2025-04-05

- New argument `cachedir` in
  [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) to set
  the location of the cache used by dependency package `ggspatial`
  ([\#708](https://github.com/adokter/bioRad/issues/708)).

## bioRad 0.9.0

CRAN release: 2025-03-31

### New features

- New function
  [`as.vp()`](http://adriaandokter.com/bioRad/dev/reference/as.vp.md) to
  convert data.frame to vertical profile object
  ([\#699](https://github.com/adokter/bioRad/issues/699)).

- New function
  [`clean_mixture()`](http://adriaandokter.com/bioRad/dev/reference/clean_mixture.md)
  for partitioning bird and insect mixtures
  ([\#700](https://github.com/adokter/bioRad/issues/700)).

- New function
  [`filter_precip()`](http://adriaandokter.com/bioRad/dev/reference/filter_precip.md)
  for posthoc removal of precipitation in vp and vpts objects
  ([\#701](https://github.com/adokter/bioRad/issues/701)).

- New argument `zoomin` for function
  [`bioRad::map()`](http://adriaandokter.com/bioRad/dev/reference/map.md)
  to increase basemap resolution
  ([\#689](https://github.com/adokter/bioRad/issues/689)).

- Added Citation File Format (cff) metadata to package
  ([\#680](https://github.com/adokter/bioRad/issues/680)).

### Bugfixes

- Corrected units specified in plot label for quantity VIR
  ([\#674](https://github.com/adokter/bioRad/issues/674)).

- Discard profiles with misspecified altitude bins in
  [`as.vpts()`](http://adriaandokter.com/bioRad/dev/reference/as.vpts.md)
  and
  [`read_vpts()`](http://adriaandokter.com/bioRad/dev/reference/read_vpts.md)
  ([\#684](https://github.com/adokter/bioRad/issues/684)).

- Correct type of gap field in vpts objects for profiles stored in ODIM
  HDF5 format (.h5)
  ([\#635](https://github.com/adokter/bioRad/issues/635),
  [\#691](https://github.com/adokter/bioRad/issues/691)).

- Interpret NA values in field DBZH in
  [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  as pixels that were not irradiated
  ([\#658](https://github.com/adokter/bioRad/issues/658)).

- Fixes a bug that prevented a data.frame to be converted to a vpts
  object with as.vpts() when profiles are not sorted in datetime and
  height ([\#692](https://github.com/adokter/bioRad/issues/692)).

- Bugfix for ignored `xlab` argument in
  [`plot.vpi()`](http://adriaandokter.com/bioRad/dev/reference/plot.vpi.md)
  and night shading fix for dealing with NA values
  ([\#693](https://github.com/adokter/bioRad/issues/693))

- Bugfix for incorrect default sd_vvp_threshold value for S-band data (2
  m/s instead of correct 1 m/s)
  ([\#695](https://github.com/adokter/bioRad/issues/695))

- Added workaround for using
  [`ifelse()`](https://rdrr.io/r/base/ifelse.html) statements in
  [`calculate_param()`](http://adriaandokter.com/bioRad/dev/reference/calculate_param.md)
  ([\#672](https://github.com/adokter/bioRad/issues/672),
  [\#673](https://github.com/adokter/bioRad/issues/673)).

- Bugfix for not working `zlim` argument in
  [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  ([\#702](https://github.com/adokter/bioRad/issues/702))

- Fixes cropping in
  [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) with
  `xlim` and `ylim` arguments
  ([\#707](https://github.com/adokter/bioRad/issues/707))

## bioRad 0.8.1

CRAN release: 2024-07-30

### Bugfixes

- dbz_all field in VPTS CSV files is now correctly mapped to DBZH field
  ([\#661](https://github.com/adokter/bioRad/issues/661)).

- Non-standard data fields are now retained in vpts objects produced
  with as.vpts()
  ([\#661](https://github.com/adokter/bioRad/issues/661)).

- Corrected the type of gap field in vpts objects to logical
  ([\#635](https://github.com/adokter/bioRad/issues/635)).

## bioRad 0.8.0

CRAN release: 2024-07-27

### New features

- ENRAM VPTS data exchange format added in package data as
  `vpts_schema.rda`

- VPTS files are now able to be validated with `validate_vpts()` which
  uses the schema to check for min/max constraint violations for
  specific fields, regex and datetime formatting

- speed up
  [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  and
  [`project_as_ppi()`](http://adriaandokter.com/bioRad/dev/reference/project_as_ppi.md)
  by using native `sf` functions
  ([\#669](https://github.com/adokter/bioRad/issues/669))

- support for tidyverse select method for polar volume and polar scan
  objects ([\#668](https://github.com/adokter/bioRad/issues/668),#460)

### Bugfixes

- Corrected the default refractive index value used in conversion of
  linear reflectivity (eta) to logarithmic reflectivity (dBZ). The
  effect is a 7% increase in animal densities in output of functions
  [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  and
  [`read_cajun()`](http://adriaandokter.com/bioRad/dev/reference/read_cajun.md)
  only ([\#642](https://github.com/adokter/bioRad/issues/642)).

- Fixed the handling of empty numeric vectors when plotting clutter maps
  ([\#655](https://github.com/adokter/bioRad/issues/655))

- Fixed warning when reading VPTS csv containing multiple values in
  `lat`, `lon`, ‘`rcs`’ or `sd_vvp_threshold`
  ([\#651](https://github.com/adokter/bioRad/issues/651))

- Fixed enabling/disabling of `single_pol` flag in
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  ([\#646](https://github.com/adokter/bioRad/issues/646))

- Updated the s3 source bucket of
  [`download_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_vpfiles.md)
  to <https://aloftdata.s3-eu-west-1.amazonaws.com>
  ([\#648](https://github.com/adokter/bioRad/issues/648))

- Fixed an error in the idw method of
  [`composite_ppi()`](http://adriaandokter.com/bioRad/dev/reference/composite_ppi.md)
  that emerged with the evolution of dependency package sp / deprecation
  of rgdal ([\#666](https://github.com/adokter/bioRad/issues/666))

## bioRad 0.7.3

CRAN release: 2023-10-20

- Replaced the `ggmap` package with `ggspatial` for map visualizations.
  This change was made as `ggmap` no longer provides reliable
  open-source basemaps without the necessity to register for an API key
  ([\#638](https://github.com/adokter/bioRad/issues/638)).

### Bugfixes

- Corrected incorrect mapping of to `dbz_all` data column in [VPTS
  CSV](https://aloftdata.eu/vpts-csv/) format to corresponding `DBZH`
  data column in bioRad vpts object
  ([\#634](https://github.com/adokter/bioRad/issues/634)).

- Improved the polar volume downloads by
  [`download_pvolfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_pvolfiles.md):
  now skips a day if there are issues with fetching instead of aborting
  the entire download
  ([\#636](https://github.com/adokter/bioRad/issues/636))

### Deprecations

- [`download_basemap()`](http://adriaandokter.com/bioRad/dev/reference/bioRad-deprecated.md)
  has been deprecated, function
  [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) now
  automatically downloads a basemap
  ([\#638](https://github.com/adokter/bioRad/issues/638)).

## bioRad 0.7.2

CRAN release: 2023-09-06

### Bugfixes

- changed default aloft bucket to aloftdata
  ([\#622](https://github.com/adokter/bioRad/issues/622))

- skip tests for
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  when vol2birdR package is not installed
  ([\#624](https://github.com/adokter/bioRad/issues/624))

- fix a bug in the calculation of flight altitude quantiles
  ([\#627](https://github.com/adokter/bioRad/issues/627)), which caused
  underestimation of flight altitude quantiles by up to one altitude
  bin.

- updates for compatibility with testthat package 3rd edition (\$630)

## bioRad 0.7.1

CRAN release: 2023-07-17

Rebuilds documentation with examples formatted as per CRAN requirements.

## bioRad 0.7.0

bioRad 0.7 includes a major backend overhaul that deprecates the use of
Docker. All Docker-dependent functionality is now included in the new
dependency package
[vol2birdR](https://cran.r-project.org/package=vol2birdR) package, which
needs to be installed as part of bioRad. All bioRad functions remain the
same, but several functions will run considerable faster.

### New features

- bioRad is now fully available on Windows.

- Simplified installation, including automatic installation of rhdf5
  from bioconductor
  ([\#464](https://github.com/adokter/bioRad/issues/464)).

- Faster implementations of functions previously dependent on Docker,
  such as
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md),
  [`apply_mistnet()`](http://adriaandokter.com/bioRad/dev/reference/apply_mistnet.md)
  and
  [`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md).

- Support for reading [VPTS CSV](https://aloftdata.eu/vpts-csv/) format
  through updated function
  [`read_vpts()`](http://adriaandokter.com/bioRad/dev/reference/read_vpts.md).
  VPTS CSV table schema included to allow offline parsing of VPTS CSV
  files as a
  [frictionless](https://CRAN.R-project.org/package=frictionless) data
  package ([\#551](https://github.com/adokter/bioRad/issues/551),
  [\#590](https://github.com/adokter/bioRad/issues/590)).

- Updated function
  [`read_vpts()`](http://adriaandokter.com/bioRad/dev/reference/read_vpts.md)
  supports reading `vp`/`vpts` data in ODIM HDF and [VPTS
  CSV](https://aloftdata.eu/vpts-csv/) format
  ([\#551](https://github.com/adokter/bioRad/issues/551),
  [\#590](https://github.com/adokter/bioRad/issues/590)).

- New function
  [`list_vpts_aloft()`](http://adriaandokter.com/bioRad/dev/reference/list_vpts_aloft.md)
  produces a list of [aloft](https://aloftdata.eu/browse/) archive URLs
  for time series of vertical profiles (`vpts`). This list of URLs can
  then be used to bulk download data using any number of external tools
  ([\#553](https://github.com/adokter/bioRad/issues/553)).

- New function `read_stdout()` replaces previous functionality of
  [`read_vpts()`](http://adriaandokter.com/bioRad/dev/reference/read_vpts.md)
  to read vol2bird stdout format. It also has a new `sep` argument
  ([\#536](https://github.com/adokter/bioRad/issues/536)) to support
  both fixed-delimited and comma-separated stdout data.

- New function `as.vpts` converts a data.frame originating from a VPTS
  CSV file into a vpts object
  ([\#555](https://github.com/adokter/bioRad/issues/555)). Inverse
  operation of
  [`as.data.frame.vpts()`](http://adriaandokter.com/bioRad/dev/reference/as.data.frame.vp.md).

- `read_pvolfiles()` now allows ODIM H5 files with missing `source`
  attribute. The functionality is similar to
  [`read_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/read_vpfiles.md),
  i.e. extracting the NOD, RAD or WMO identifier, otherwise using
  `unknown` (2f6935c).

- [`bind_into_vpts()`](http://adriaandokter.com/bioRad/dev/reference/bind_into_vpts.md)
  now works for vp and vpts objects with different heights
  ([\#343](https://github.com/adokter/bioRad/issues/343)).

- Faster parallel mistnet runs
  (<https://github.com/adokter/vol2birdR/issues/16>).

### Bugfixes

- Fix bug in height integration, which excluded the lowest bins for
  certain low altitude radars
  ([\#534](https://github.com/adokter/bioRad/issues/534)).

### Deprecations

- Argument `local_install` in
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  and
  [`apply_mistnet()`](http://adriaandokter.com/bioRad/dev/reference/apply_mistnet.md)
  is now deprecated.

- Functions
  [`check_docker()`](http://adriaandokter.com/bioRad/dev/reference/bioRad-deprecated.md)
  and
  [`update_docker()`](http://adriaandokter.com/bioRad/dev/reference/bioRad-deprecated.md)
  have been deprecated.

- Function
  [`vol2bird_version()`](http://adriaandokter.com/bioRad/dev/reference/bioRad-deprecated.md)
  has been migrated to package vol2birdR and can be accessed by
  [`vol2birdR::vol2bird_version()`](https://rdrr.io/pkg/vol2birdR/man/vol2bird_version.html).

- Dependency `maptools` has been replaced with
  [suntools](https://github.com/adokter/suntools), `rgdal` has been
  removed in accordance with the evolution of `sp` and the [imminent
  archiving](https://r-spatial.org/r/2023/05/15/evolution4.html) of
  `rgdal`.

- Function
  [`as.data.frame.vpts()`](http://adriaandokter.com/bioRad/dev/reference/as.data.frame.vp.md)
  has output column names `lat`, `lon`, `antenna_height` renamed to
  `radar_latitude`, `radar_longitude`, `radar_height` for compatibility
  with the [VPTS CSV](https://aloftdata.eu/vpts-csv/) data format. The
  function also outputs an additional column `radar_wavelength`
  ([\#609](https://github.com/adokter/bioRad/issues/609)).

## bioRad 0.6.1

CRAN release: 2022-08-30

Rebuilds the documentation using roxygen2 for compatibility with HTML5
(a CRAN requirement).

## bioRad 0.6.0

CRAN release: 2022-05-09

Introduces a number of new functions and parameters and includes
bugfixes. All issues included in this release can be found
[here](https://github.com/adokter/bioRad/milestone/8?closed=1).

### New functions

- New function
  [`attribute_table()`](http://adriaandokter.com/bioRad/dev/reference/attribute_table.md)
  to quickly tabulate scan attributes
  ([\#365](https://github.com/adokter/bioRad/issues/365)).

- New function
  [`get_iris_raw_task()`](http://adriaandokter.com/bioRad/dev/reference/get_iris_raw_task.md)
  to returns `task` from IRIS files
  ([\#411](https://github.com/adokter/bioRad/issues/411)).

- New functions
  [`is.vpi()`](http://adriaandokter.com/bioRad/dev/reference/summary.vpi.md)
  and [`summary()`](https://rdrr.io/r/base/summary.html) for vpi objects
  ([\#380](https://github.com/adokter/bioRad/issues/380),
  [\#405](https://github.com/adokter/bioRad/issues/405)).

- New function
  [`write_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/write_pvolfile.md)
  to write polar volumes to ODIM hdf5 format
  ([\#470](https://github.com/adokter/bioRad/issues/470),
  [\#471](https://github.com/adokter/bioRad/issues/471)).

- New function
  [`download_pvolfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_pvolfiles.md)
  to download NEXRAD polar volume files from Amazon Web Services
  ([\#41](https://github.com/adokter/bioRad/issues/41),
  [\#487](https://github.com/adokter/bioRad/issues/487))

- New functions
  [`Math.pvol()`](http://adriaandokter.com/bioRad/dev/reference/operators.md),
  [`Math.scan()`](http://adriaandokter.com/bioRad/dev/reference/operators.md),
  [`Ops.pvol()`](http://adriaandokter.com/bioRad/dev/reference/operators.md),[`Ops.scan()`](http://adriaandokter.com/bioRad/dev/reference/operators.md),[`Ops.param()`](http://adriaandokter.com/bioRad/dev/reference/operators.md):
  standard mathematical operations for `param`, `scan` and `pvol`
  objects.

### New features and bugfixes

- [`apply_mistnet()`](http://adriaandokter.com/bioRad/dev/reference/apply_mistnet.md),
  [`nexrad_to_odim()`](http://adriaandokter.com/bioRad/dev/reference/nexrad_to_odim.md)
  and
  [`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md)
  can now use local vol2bird installation
  ([\#416](https://github.com/adokter/bioRad/issues/416),
  [\#398](https://github.com/adokter/bioRad/issues/398)).

- [`beam_profile_overlap()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile_overlap.md)
  and
  [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  bugfix that affected profiles with only `NA` values
  ([\#396](https://github.com/adokter/bioRad/issues/396)).

- [`calculate_param()`](http://adriaandokter.com/bioRad/dev/reference/calculate_param.md)
  now also works on PPIs
  ([\#316](https://github.com/adokter/bioRad/issues/316)) + bugfix for
  lost attributes
  ([\#401](https://github.com/adokter/bioRad/issues/401)) and long
  expressions without name
  ([\#399](https://github.com/adokter/bioRad/issues/399)).

- [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  now sets default `range_max` to 35km
  ([\#206](https://github.com/adokter/bioRad/issues/206)) + has a new
  optional parameter `height_quantile` to calculate flight altitude
  quantiles ([\#485](https://github.com/adokter/bioRad/issues/485)) +
  parameters for local mistnet
  ([\#418](https://github.com/adokter/bioRad/issues/418),
  [\#488](https://github.com/adokter/bioRad/issues/488)).

- [`composite_ppi()`](http://adriaandokter.com/bioRad/dev/reference/composite_ppi.md)
  now composites multiple PPI parameters at once
  ([\#390](https://github.com/adokter/bioRad/issues/390),
  [\#393](https://github.com/adokter/bioRad/issues/393)) + bugfix for
  `ylim` parameter
  ([\#389](https://github.com/adokter/bioRad/issues/389)).

- [`get_scan()`](http://adriaandokter.com/bioRad/dev/reference/get_scan.md)
  warns when multiple scans with the same elevation are equally close to
  the requested elevation and add option to return all
  ([\#414](https://github.com/adokter/bioRad/issues/414)).

- [`integrate_profile()`](http://adriaandokter.com/bioRad/dev/reference/integrate_profile.md)
  now sets default `interval_max` to 1h
  ([\#481](https://github.com/adokter/bioRad/issues/481)) and has a new
  argument `interval_replace`. The function can now integrate at
  altitude resolutions smaller than the altitude bin spacing. New option
  `antenna` for `alt_min` parameter, setting the altitude of the antenna
  as the minimum altitude
  ([\#472](https://github.com/adokter/bioRad/issues/472)). Directional
  angles are now always mapped to the 0-360 degree domain
  ([\#489](https://github.com/adokter/bioRad/issues/489)).

- [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  and other functions are sped up by avoiding duplicate input argument
  checking ([\#358](https://github.com/adokter/bioRad/issues/358)) +
  radar name now added to output
  ([\#425](https://github.com/adokter/bioRad/issues/425),
  [\#443](https://github.com/adokter/bioRad/issues/443)). Bugfix
  affecting profiles consisting of primarily NA/NaN values
  ([\#415](https://github.com/adokter/bioRad/issues/415)), which are now
  treated as zeros.

- [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) bugfix
  for transparency
  ([\#438](https://github.com/adokter/bioRad/issues/438)) and base layer
  ([\#468](https://github.com/adokter/bioRad/issues/468)).

- [`plot.vpi()`](http://adriaandokter.com/bioRad/dev/reference/plot.vpi.md)
  now has an `elev` parameter
  ([\#412](https://github.com/adokter/bioRad/issues/412)).

- [`plot.vpts()`](http://adriaandokter.com/bioRad/dev/reference/plot.vpts.md)
  now supports custom color scales
  ([\#444](https://github.com/adokter/bioRad/issues/444)). It can plot
  speed and direction as colours
  ([\#424](https://github.com/adokter/bioRad/issues/424)) + height
  offset fixed in plots
  ([\#198](https://github.com/adokter/bioRad/issues/198)) + bugfix
  negative `zlim` values
  ([\#402](https://github.com/adokter/bioRad/issues/402)).

- [`read_cajun()`](http://adriaandokter.com/bioRad/dev/reference/read_cajun.md)
  bugfix for incorrect conversion of linear eta to reflectivity
  ([\#403](https://github.com/adokter/bioRad/issues/403)).

- [`read_vpts()`](http://adriaandokter.com/bioRad/dev/reference/read_vpts.md)
  bugfix for missing height attribute
  ([\#409](https://github.com/adokter/bioRad/issues/409)).

- [`regularize_vpts()`](http://adriaandokter.com/bioRad/dev/reference/regularize_vpts.md)’s
  `fill` parameter now allows to specify a time interval over which to
  perform nearest neighbor interpolation to fill gaps of missing profile
  data. ([\#475](https://github.com/adokter/bioRad/issues/475)) + bugfix
  for `max_interval` parameter
  ([\#480](https://github.com/adokter/bioRad/issues/480),
  [\#484](https://github.com/adokter/bioRad/issues/484),
  [\#475](https://github.com/adokter/bioRad/issues/475)).

- [`scan_to_spatial()`](http://adriaandokter.com/bioRad/dev/reference/scan_to_spatial.md)
  now creates points for cell centers
  ([\#430](https://github.com/adokter/bioRad/issues/430)).

### Additional features

- The package now accounts for `rstart` and `astart` from the ODIM
  specification ([\#434](https://github.com/adokter/bioRad/issues/434)).

- Clearly indicate when a speed is ground speed
  ([\#462](https://github.com/adokter/bioRad/issues/462)).

- Improved documentation and unit tests for a number of functions thanks
  to an online bioRad sprint (June 2021).

- GitHub Actions are set up to automatically test changes
  ([\#428](https://github.com/adokter/bioRad/issues/428)).

## bioRad 0.5.2

CRAN release: 2020-05-11

This release is primarily a hotfix for R version 4.0
([\#375](https://github.com/adokter/bioRad/issues/375)). All issues
included in this release can be found
[here](https://github.com/adokter/bioRad/milestone/11?closed=1). New
features and improvements include:

- [`regularize_vpts()`](http://adriaandokter.com/bioRad/dev/reference/regularize_vpts.md)
  is now much faster, and chooses more intuitive starting and ending
  point of the regularized grid, e.g. projecting on half hour grid will
  have time series start on the nearest half hour
  ([\#332](https://github.com/adokter/bioRad/issues/332)).

- [`regularize_vpts()`](http://adriaandokter.com/bioRad/dev/reference/regularize_vpts.md)
  has new option `keep_timestamp`, which allows individual profiles to
  keep there original timestamp instead of the timestamp of the
  regularized grid.

- [`sunrise()`](http://adriaandokter.com/bioRad/dev/reference/sunrise_sunset.md)/[`sunset()`](http://adriaandokter.com/bioRad/dev/reference/sunrise_sunset.md)
  have improved documentation
  ([\#180](https://github.com/adokter/bioRad/issues/180)) and new option
  `force_tz`
  ([4968019](https://github.com/adokter/bioRad/commit/4968019)).

- [`check_night()`](http://adriaandokter.com/bioRad/dev/reference/check_night.md)
  has new option `offset`, which allows day/night transition to be
  shifted by a temporal offset
  ([\#338](https://github.com/adokter/bioRad/issues/338)). For example,
  this is useful when selecting night time profiles that start a
  specific number of hours after sunset.

- [`check_night()`](http://adriaandokter.com/bioRad/dev/reference/check_night.md)
  now works for vpi objects
  ([23def64](https://github.com/adokter/bioRad/commit/23def64)).

- [`filter_vpts()`](http://adriaandokter.com/bioRad/dev/reference/filter_vpts.md)
  allows to select for day and night in vpts using new arguments
  `night`, `elev` and `offset`, based on functionality of
  [`check_night()`](http://adriaandokter.com/bioRad/dev/reference/check_night.md)
  ([\#345](https://github.com/adokter/bioRad/issues/345)).

- New functions
  [`noy()`](http://adriaandokter.com/bioRad/dev/reference/doy_noy.md)
  and
  [`doy()`](http://adriaandokter.com/bioRad/dev/reference/doy_noy.md) to
  determine which night or day of the year a profile belongs to
  ([\#346](https://github.com/adokter/bioRad/issues/346)).

- [`as.data.frame.vp()`](http://adriaandokter.com/bioRad/dev/reference/as.data.frame.vp.md)
  now has separate function page and deprecated parameter `quantities`
  ([\#364](https://github.com/adokter/bioRad/issues/364)).

- [`get_quantity()`](http://adriaandokter.com/bioRad/dev/reference/get_quantity.md)
  now has improved documentation and allows to return height
  ([\#352](https://github.com/adokter/bioRad/issues/352)).

- [`dim()`](https://rdrr.io/r/base/dim.html) now returns dimensions in a
  different, more logical order for pvol and vpts objects
  ([\#355](https://github.com/adokter/bioRad/issues/355)).

- Improved documentation and unit tests for a number of functions thanks
  to an online bioRad sprint (April 2020).

- Bugfixes ([\#330](https://github.com/adokter/bioRad/issues/330),
  [\#368](https://github.com/adokter/bioRad/issues/368)).

## bioRad 0.5.1

CRAN release: 2020-04-01

Minor bugfixes. All issues included in this release can be found
[here](https://github.com/adokter/bioRad/pull/334). This release
primarily fixes a bug that will become effective once R version 4.0 is
released.

- Fixes a conflict due to new raw data format introduced in R version
  4.0 ([\#331](https://github.com/adokter/bioRad/issues/331)).

- Corrects incorrect values in reading of correlation coefficient values
  RHOHV ([\#328](https://github.com/adokter/bioRad/issues/328)).

- Bugfix in
  [`read_cajun()`](http://adriaandokter.com/bioRad/dev/reference/read_cajun.md)
  which introduced incorrect height column during refactoring in bioRad
  0.5.0 release
  ([93ad0a4](https://github.com/adokter/bioRad/commit/93ad0a4)).

- Bugfix that fixes the mapping by
  [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) of
  composites of composites (ppi’s produced after repeated application of
  [`composite_ppi()`](http://adriaandokter.com/bioRad/dev/reference/composite_ppi.md),
  [a5c9048](https://github.com/adokter/bioRad/commit/a5c9048),
  [043aa73](https://github.com/adokter/bioRad/commit/043aa73)).

- Minor bug fixes, and addressing conflicts with CRAN dependencies.

- Extend the functionality of
  [`composite_ppi()`](http://adriaandokter.com/bioRad/dev/reference/composite_ppi.md)
  and improve its documentation (partial fix of
  [\#59](https://github.com/adokter/bioRad/issues/59)).

- Fix a bug in the color legend of
  [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) when
  providing a custom color scale with palette argument
  ([\#324](https://github.com/adokter/bioRad/issues/324)).

- Minor documentation improvements.

## bioRad 0.5.0

CRAN release: 2020-02-19

New CRAN release. All issues included in this release can be found
[here](https://github.com/adokter/bioRad/milestone/6?closed=1).

### New features

- [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  is a new function to estimate spatial images of vertically integrated
  density and reflectivity. This function produces an `ppi` image
  showing the density of animals on the earth’s surface, corrected for
  the changing overlap between the radar beams and animal layer with
  distance from the radar. See [Kranstauber et
  al. 2020](https://doi.org/10.3390/rs12040635) for methodology and
  [this
  vignette](https://adriaandokter.com/bioRad/articles/range_correction.html)
  for examples.

- [`apply_mistnet()`](http://adriaandokter.com/bioRad/dev/reference/apply_mistnet.md)
  is a new function to apply the convolution neural network “MistNet” on
  pvolfiles to separate biological and meteorological signals (see [Lin
  et al. 2019](https://doi.org/10.1111/2041-210X.13280)). Results can be
  readily visualized with
  [`plot.ppi()`](http://adriaandokter.com/bioRad/dev/reference/plot.ppi.md)
  and
  [`plot.scan()`](http://adriaandokter.com/bioRad/dev/reference/plot.scan.md).
  MistNet is now also a segmentation option in
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  ([\#262](https://github.com/adokter/bioRad/issues/262)).

- [`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md)
  and
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  now read Vaisala IRIS RAW format directly, helpful for countries like
  🇨🇦🇫🇮🇨🇴🇵🇹 ([\#222](https://github.com/adokter/bioRad/issues/222)).
  bioRad now also reads files containing single elevation scans and
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  can calculate profiles from multiple files containing single elevation
  scans ([\#221](https://github.com/adokter/bioRad/issues/221)).

### Newer/faster conversions

- [`calculate_param()`](http://adriaandokter.com/bioRad/dev/reference/calculate_param.md)
  is a new function to calculate parameters from existing parameters,
  e.g. reflectivity `eta` from reflectivity factor `DBZH`
  ([\#287](https://github.com/adokter/bioRad/issues/287)).

- [`scan_to_raster()`](http://adriaandokter.com/bioRad/dev/reference/scan_to_raster.md)
  is a new function to convert a `scan` into a `RasterBrick` compatible
  with package [raster](https://cran.r-project.org/package=raster)
  ([\#238](https://github.com/adokter/bioRad/issues/238)).

- [`scan_to_spatial()`](http://adriaandokter.com/bioRad/dev/reference/scan_to_spatial.md)
  is a new function to convert a `scan` into a `SpatialPointsDataFrame`
  compatible with package [sp](https://cran.r-project.org/package=sp)
  ([\#238](https://github.com/adokter/bioRad/issues/238)).

- [`project_as_ppi()`](http://adriaandokter.com/bioRad/dev/reference/project_as_ppi.md)
  is now much faster
  ([e420e5d](https://github.com/adokter/bioRad/commit/e420e5d)) and
  accounts for earth’s curvature
  ([820e85f](https://github.com/adokter/bioRad/commit/820e85f)).

### New functions describing the radar beam geometry

- [`beam_distance()`](http://adriaandokter.com/bioRad/dev/reference/beam_distance.md)
  and
  [`beam_range()`](http://adriaandokter.com/bioRad/dev/reference/beam_range.md)
  are new functions to relate range (i.e. slant range), distance
  (i.e. down range) and height of the radar beam.

- [`beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile.md)
  is a new function to calculate for a set of beam elevations the
  altitudinal normalized distribution of radiated energy by those beams.

- [`beam_profile_overlap()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile_overlap.md)
  is a new function to calculate the distribution overlap (in terms of
  Bhattacharyya distance) between a vertical profile (`vp`) and the
  vertical radiation profile of a set of emitted radar beams (given by
  [`beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile.md)).

### Additional features

- Custom color scales in
  [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) and
  `plot.ppi` ([\#318](https://github.com/adokter/bioRad/issues/318)).

- [`nyquist_velocity()`](http://adriaandokter.com/bioRad/dev/reference/nyquist_velocity.md)
  is a new function to calculate the unambiguous velocity of Doppler
  radar from its pulse repetition frequency/frequencies
  ([\#208](https://github.com/adokter/bioRad/issues/208)).

- [`filter_vpts()`](http://adriaandokter.com/bioRad/dev/reference/filter_vpts.md)
  is a new function simplifying the selection of time ranges and
  instances in vertical profile time series (`vpts`)
  ([\#241](https://github.com/adokter/bioRad/issues/241)).

- The definition of height-integrated velocity is now corrected
  ([\#232](https://github.com/adokter/bioRad/issues/232),
  [\#233](https://github.com/adokter/bioRad/issues/233),
  [72be6d1](https://github.com/adokter/bioRad/commit/72be6d1)).

- Improved documentation of how mtr can be calculated from vid, u, v
  ([6dce625](https://github.com/adokter/bioRad/commit/6dce625)).

- [`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md)
  now also reads quantities DBZ, TH, T, because these often occur in
  European data and are relevant for biological analysis (note that DBZ
  and T are not ODIM-compliant names)
  ([5db08bd](https://github.com/adokter/bioRad/commit/5db08bd)).

- [`plot.scan()`](http://adriaandokter.com/bioRad/dev/reference/plot.scan.md)
  now has correct ordering of rays
  ([\#285](https://github.com/adokter/bioRad/issues/285)).

- [`plot.vpts()`](http://adriaandokter.com/bioRad/dev/reference/plot.vpts.md)
  now has correctly positioned and rounded speed barbs
  ([\#277](https://github.com/adokter/bioRad/issues/277),
  [\#244](https://github.com/adokter/bioRad/issues/244)).

- [`integrate_profile()`](http://adriaandokter.com/bioRad/dev/reference/integrate_profile.md)’s
  default unit of interval argument has been changed to seconds
  ([\#234](https://github.com/adokter/bioRad/issues/234)).

- [`project_as_ppi()`](http://adriaandokter.com/bioRad/dev/reference/project_as_ppi.md)’s
  default projection is now on earth’s surface
  ([\#280](https://github.com/adokter/bioRad/issues/280)).

- `height` is now the default quantity denoting height above mean sea
  level. `HGHT` is deprecated
  ([\#273](https://github.com/adokter/bioRad/issues/273)).

- [`plot.vpts()`](http://adriaandokter.com/bioRad/dev/reference/plot.vpts.md)
  and
  [`plot.vp()`](http://adriaandokter.com/bioRad/dev/reference/plot.vp.md)
  plots are shifted up by half the height interval to reflect that
  height refers to the bottom of the height interval
  ([\#277](https://github.com/adokter/bioRad/issues/277),
  [\#198](https://github.com/adokter/bioRad/issues/198)).

- [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)’s
  default `sd_vvp_threshold` parameter value at S-band is now 1 m/s
  ([\#93](https://github.com/adokter/bioRad/issues/93)).

- And many small bug fixes and documentation improvements.

## bioRad 0.4.0

CRAN release: 2018-12-14

First release on CRAN! All issues included in this release can be found
[here](https://github.com/adokter/bioRad/milestone/4?closed=1).

- [`get_param()`](http://adriaandokter.com/bioRad/dev/reference/get_param.md)
  added as new function to get parameters from scan
  ([\#132](https://github.com/adokter/bioRad/issues/132)).

- [`download_basemap()`](http://adriaandokter.com/bioRad/dev/reference/bioRad-deprecated.md)
  now uses Stamen basemaps by default (parameter `source`), so users do
  not need to request an API key for the previous default Google Maps
  ([\#163](https://github.com/adokter/bioRad/issues/163)).

- [`download_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_vpfiles.md)
  and
  [`select_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/select_vpfiles.md)
  now use 5 letter radar codes (parameter `radars` instead of `radar`
  and `country`), allow to skip already downloaded files (parameter
  `overwrite = TRUE`) and have improved download and error messages
  ([\#176](https://github.com/adokter/bioRad/issues/176)).

- [`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md),
  [`nexrad_to_odim()`](http://adriaandokter.com/bioRad/dev/reference/nexrad_to_odim.md)
  and
  [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  (all Docker dependent) can now read files from directories containing
  a space ([\#162](https://github.com/adokter/bioRad/issues/162)).

- Details for deprecated functions `mt()`, `mtr()`, `cmt()` are now
  displayed with function
  ([\#166](https://github.com/adokter/bioRad/issues/166)).

- [README](https://adriaandokter.com/bioRad/) reviewed for easier
  installation and usage
  ([\#155](https://github.com/adokter/bioRad/issues/155)).

- [Get started
  vignette](https://adriaandokter.com/bioRad/articles/bioRad.html) is
  now based on figure and workflow from [Dokter et
  al.](https://doi.org/10.1111/ecog.04028)
  ([\#168](https://github.com/adokter/bioRad/issues/168)).

- [Introductory exercises
  vignette](https://adriaandokter.com/bioRad/articles/rad_aero_19.html)
  renamed to `rad_aero_19.Rmd` since it is based on the 3d Radar
  Aeroecology Training School.

- bioRad now has a [code of
  conduct](https://adriaandokter.com/bioRad/CODE_OF_CONDUCT.html) and
  [contributing
  guidelines](https://adriaandokter.com/bioRad/CONTRIBUTING.html)
  ([\#145](https://github.com/adokter/bioRad/issues/145)).

## bioRad 0.3.0

Release consistent with and in preparation of the bioRad methods paper
(<https://doi.org/10.1111/ecog.04028>). All issues included in this
release can be found
[here](https://github.com/adokter/bioRad/milestone/1?closed=1).

- Functions ([\#84](https://github.com/adokter/bioRad/issues/84)),
  arguments ([\#112](https://github.com/adokter/bioRad/issues/112)) and
  objects ([\#80](https://github.com/adokter/bioRad/issues/80)) have
  been renamed to be consistent
  ([\#51](https://github.com/adokter/bioRad/issues/51)). Deprecated
  functions will remain functional for now, but we will trigger a
  warning: **we advise to use the new functions names**. See the lists
  for [current functions](https://adriaandokter.com/bioRad/reference/)
  and [deprecated
  functions](https://adriaandokter.com/bioRad/reference/bioRad-deprecated.html).

- [`integrate_profile()`](http://adriaandokter.com/bioRad/dev/reference/integrate_profile.md)
  replaces the functionality of `cmt()`
  ([\#75](https://github.com/adokter/bioRad/issues/75)) and `mt()`
  ([\#76](https://github.com/adokter/bioRad/issues/76)).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) can now be
  used for scans ([\#71](https://github.com/adokter/bioRad/issues/71)),
  e.g. `plot(example_scan)`.

- Functions are [organized in
  sections](https://adriaandokter.com/bioRad/reference/) on the website
  ([\#110](https://github.com/adokter/bioRad/issues/110)).

- Changelog section (this page) has been added to website
  ([\#144](https://github.com/adokter/bioRad/issues/144)).

- Package R code is reorganized as one function = one file for easier
  maintenance ([\#50](https://github.com/adokter/bioRad/issues/50)).

- First tests are included for some functions.

- Contributors ([\#90](https://github.com/adokter/bioRad/issues/90)) and
  citation ([\#141](https://github.com/adokter/bioRad/issues/141)) have
  been updated.

- bioRad now has a hex logo
  ([\#137](https://github.com/adokter/bioRad/issues/137)). ✨
