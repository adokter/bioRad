# Package index

## Reading radar data

Functions to read polar volume (`pvol`) data.

- [`download_pvolfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_pvolfiles.md)
  :

  Download polar volume (`pvol`) files from the NEXRAD archive

- [`is.pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/is.pvolfile.md)
  :

  Check if a file is a polar volume (`pvol`)

- [`read_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/read_pvolfile.md)
  :

  Read a polar volume (`pvol`) from file

- [`write_pvolfile()`](http://adriaandokter.com/bioRad/dev/reference/write_pvolfile.md)
  :

  Write a polar volume (`pvol`) object to ODIM HDF5 file

- [`get_iris_raw_task()`](http://adriaandokter.com/bioRad/dev/reference/get_iris_raw_task.md)
  :

  Check the `task` type of an IRIS RAW file

- [`get_odim_object_type()`](http://adriaandokter.com/bioRad/dev/reference/get_odim_object_type.md)
  :

  Check the `data` type of an ODIM HDF5 file

- [`nexrad_to_odim()`](http://adriaandokter.com/bioRad/dev/reference/nexrad_to_odim.md)
  : Convert a NEXRAD polar volume file to an ODIM polar volume file

- [`get_elevation_angles()`](http://adriaandokter.com/bioRad/dev/reference/get_elevation_angles.md)
  :

  Get elevation angles of a polar volume (`pvol`), scan (`scan`) or
  parameter (`param`)

- [`summary(`*`<pvol>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.pvol.md)
  [`print(`*`<pvol>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.pvol.md)
  [`is.pvol()`](http://adriaandokter.com/bioRad/dev/reference/summary.pvol.md)
  [`dim(`*`<pvol>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.pvol.md)
  :

  Inspect a polar volume (`pvol`)

- [`select(`*`<scan>`*`)`](http://adriaandokter.com/bioRad/dev/reference/tidyverse.md)
  [`select(`*`<pvol>`*`)`](http://adriaandokter.com/bioRad/dev/reference/tidyverse.md)
  : Tidyverse methods for bioRad objects

## Inspecting radar scans

Functions to inspect and plot a polar scan (`scan`) from a polar volume
(`pvol`).

- [`example_scan`](http://adriaandokter.com/bioRad/dev/reference/example_scan.md)
  :

  Scan (`scan`) example

- [`get_elevation_angles()`](http://adriaandokter.com/bioRad/dev/reference/get_elevation_angles.md)
  :

  Get elevation angles of a polar volume (`pvol`), scan (`scan`) or
  parameter (`param`)

- [`get_scan()`](http://adriaandokter.com/bioRad/dev/reference/get_scan.md)
  :

  Get a scan (`scan`) from a polar volume (`pvol`)

- [`plot(`*`<scan>`*`)`](http://adriaandokter.com/bioRad/dev/reference/plot.scan.md)
  :

  Plot a scan (`scan`) in polar coordinates

- [`summary(`*`<scan>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.scan.md)
  [`print(`*`<scan>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.scan.md)
  [`is.scan()`](http://adriaandokter.com/bioRad/dev/reference/summary.scan.md)
  [`dim(`*`<scan>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.scan.md)
  :

  Inspect a scan (`scan`)

- [`select(`*`<scan>`*`)`](http://adriaandokter.com/bioRad/dev/reference/tidyverse.md)
  [`select(`*`<pvol>`*`)`](http://adriaandokter.com/bioRad/dev/reference/tidyverse.md)
  : Tidyverse methods for bioRad objects

- [`get_param()`](http://adriaandokter.com/bioRad/dev/reference/get_param.md)
  :

  Get a parameter (`param`) from a scan (`scan`)

- [`summary(`*`<param>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.param.md)
  [`print(`*`<param>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.param.md)
  [`is.param()`](http://adriaandokter.com/bioRad/dev/reference/summary.param.md)
  :

  Inspect a parameter (`param`)

## Manipulating radar scans

Functions to manipulate radar images (`scan`, `param`, `ppi`) and to
identify precipitation.

- [`add_param()`](http://adriaandokter.com/bioRad/dev/reference/add_param.md)
  : Add scan parameter from a georeferenced raster.
- [`apply_mistnet()`](http://adriaandokter.com/bioRad/dev/reference/apply_mistnet.md)
  : Apply MistNet segmentation to a polar volume
- [`calculate_param()`](http://adriaandokter.com/bioRad/dev/reference/calculate_param.md)
  : Calculate a new scan parameter
- [`Math(`*`<scan>`*`)`](http://adriaandokter.com/bioRad/dev/reference/operators.md)
  [`Math(`*`<pvol>`*`)`](http://adriaandokter.com/bioRad/dev/reference/operators.md)
  [`Ops(`*`<param>`*`)`](http://adriaandokter.com/bioRad/dev/reference/operators.md)
  [`Ops(`*`<scan>`*`)`](http://adriaandokter.com/bioRad/dev/reference/operators.md)
  [`Ops(`*`<pvol>`*`)`](http://adriaandokter.com/bioRad/dev/reference/operators.md)
  : Mathematical and arithmetic operations on param's, scan's and pvol's

## Plotting radar scans

Functions to plot a polar volume (`pvol`), scan (`scan`) or parameter
(`param`) on a grid or basemap as a plan position indicator (`ppi`).

- [`project_as_ppi()`](http://adriaandokter.com/bioRad/dev/reference/project_as_ppi.md)
  :

  Project a scan (`scan`) or parameter (`param`) to a plan position
  indicator (`ppi`)

- [`composite_ppi()`](http://adriaandokter.com/bioRad/dev/reference/composite_ppi.md)
  :

  Create a composite of multiple plan position indicators (`ppi`)

- [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)
  :

  Calculate a plan position indicator (`ppi`) of vertically integrated
  density adjusted for range effects

- [`map()`](http://adriaandokter.com/bioRad/dev/reference/map.md) :

  Map a plan position indicator (`ppi`) on a map

- [`plot(`*`<ppi>`*`)`](http://adriaandokter.com/bioRad/dev/reference/plot.ppi.md)
  :

  Plot a plan position indicator (`ppi`)

- [`` `[`( ``*`<ppi>`*`)`](http://adriaandokter.com/bioRad/dev/reference/sub-.ppi.md)
  :

  Subset a plan position indicator (`ppi`)

- [`summary(`*`<ppi>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.ppi.md)
  [`print(`*`<ppi>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.ppi.md)
  [`is.ppi()`](http://adriaandokter.com/bioRad/dev/reference/summary.ppi.md)
  [`dim(`*`<ppi>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.ppi.md)
  :

  Inspect a plan position indicator (`ppi`)

- [`scan_to_raster()`](http://adriaandokter.com/bioRad/dev/reference/scan_to_raster.md)
  : Convert a polar scan into a raster

- [`scan_to_spatial()`](http://adriaandokter.com/bioRad/dev/reference/scan_to_spatial.md)
  : Convert a polar scan into a spatial object.

## Creating vertical profiles of biological targets

Functions to process weather radar data (`pvol`) into vertical profiles
(`vp`) of biological targets.

- [`calculate_vp()`](http://adriaandokter.com/bioRad/dev/reference/calculate_vp.md)
  :

  Calculate a vertical profile (`vp`) from a polar volume (`pvol`) file

## Reading vertical profile data

Functions to download, read, inspect and plot vertical profile (`vp`)
data.

- [`download_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/download_vpfiles.md)
  **\[superseded\]** :

  Download vertical profile (`vp`) files from the ENRAM data repository

- [`is.vpfile()`](http://adriaandokter.com/bioRad/dev/reference/is.vpfile.md)
  :

  Check if a file is a vertical profile (`vp`)

- [`list_vpts_aloft()`](http://adriaandokter.com/bioRad/dev/reference/list_vpts_aloft.md)
  **\[superseded\]** :

  List aloft urls for time series of vertical profiles (`vpts`) of radar
  stations

- [`read_cajun()`](http://adriaandokter.com/bioRad/dev/reference/read_cajun.md)
  :

  Read a vertical profile (`vp`) from UMASS Cajun text file

- [`read_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/read_vpfiles.md)
  :

  Read a vertical profile (`vp`) or a list of vertical profiles (`vp`)
  from files

- [`read_vpts()`](http://adriaandokter.com/bioRad/dev/reference/read_vpts.md)
  :

  Read time series of vertical profiles (`vpts`) from file(s)

- [`select_vpfiles()`](http://adriaandokter.com/bioRad/dev/reference/select_vpfiles.md)
  :

  Select vertical profile (`vp`) files from computer

## Inspecting vertical profile data

Functions to inspect and plot a vertical profiles (`vp`) and time series
(`vpts`).

- [`example_vp`](http://adriaandokter.com/bioRad/dev/reference/example_vp.md)
  :

  Vertical profile (`vp`) example

- [`get_quantity()`](http://adriaandokter.com/bioRad/dev/reference/get_quantity.md)
  :

  Get a quantity from a vertical profile (`vp`) or time series of
  vertical profiles (`vpts`)

- [`plot(`*`<vp>`*`)`](http://adriaandokter.com/bioRad/dev/reference/plot.vp.md)
  :

  Plot a vertical profile (`vp`)

- [`summary(`*`<vp>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)
  [`print(`*`<vp>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)
  [`is.vp()`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)
  [`dim(`*`<vp>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)
  :

  Inspect a vertical profile (`vp`)

- [`example_vpts`](http://adriaandokter.com/bioRad/dev/reference/example_vpts.md)
  :

  Time series of vertical profiles (`vpts`) example

- [`plot(`*`<vpts>`*`)`](http://adriaandokter.com/bioRad/dev/reference/plot.vpts.md)
  :

  Plot a time series of vertical profiles (`vpts`)

- [`` `[`( ``*`<vpts>`*`)`](http://adriaandokter.com/bioRad/dev/reference/sub-.vpts.md)
  :

  Subset a time series of vertical profiles (`vpts`)

- [`summary(`*`<vpts>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.vpts.md)
  [`print(`*`<vpts>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.vpts.md)
  [`is.vpts()`](http://adriaandokter.com/bioRad/dev/reference/summary.vpts.md)
  [`dim(`*`<vpts>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.vpts.md)
  :

  Inspect a time series of vertical profiles (`vpts`)

## Manipulating vertical profile data

Functions to combine vertical profiles (`vp`) into time series (`vpts`)
and to post-process these.

- [`bind_into_vpts()`](http://adriaandokter.com/bioRad/dev/reference/bind_into_vpts.md)
  :

  Bind vertical profiles (`vp`) into time series (`vpts`)

- [`c(`*`<vp>`*`)`](http://adriaandokter.com/bioRad/dev/reference/c.vp.md)
  :

  Concatenate vertical profiles (`vp`) into a list of vertical profiles

- [`clean_mixture()`](http://adriaandokter.com/bioRad/dev/reference/clean_mixture.md)
  : Partition mixtures of animals using assumptions on airspeeds.

- [`filter_precip()`](http://adriaandokter.com/bioRad/dev/reference/filter_precip.md)
  : Posthoc precipitation filter

- [`filter_vpts()`](http://adriaandokter.com/bioRad/dev/reference/filter_vpts.md)
  : Filter a time series of vertical profiles ('vpts').

- [`` `rcs<-`() ``](http://adriaandokter.com/bioRad/dev/reference/rcs-set.md)
  : Set radar cross section

- [`regularize_vpts()`](http://adriaandokter.com/bioRad/dev/reference/regularize_vpts.md)
  :

  Regularize a time series of vertical profiles (`vpts`) on a regular
  time grid

- [`` `sd_vvp_threshold<-`() ``](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold-set.md)
  : Set threshold of the radial velocity standard deviation

- [`as.data.frame(`*`<vp>`*`)`](http://adriaandokter.com/bioRad/dev/reference/as.data.frame.vp.md)
  [`as.data.frame(`*`<vpts>`*`)`](http://adriaandokter.com/bioRad/dev/reference/as.data.frame.vp.md)
  :

  Convert a vertical profile (`vp`) or time series of vertical profiles
  (`vpts`) to a data frame

- [`as.vp()`](http://adriaandokter.com/bioRad/dev/reference/as.vp.md) :
  Convert a dataframe into a vp object

- [`as.vpts()`](http://adriaandokter.com/bioRad/dev/reference/as.vpts.md)
  : Convert a dataframe into a vpts object

## Integrating vertical profiles

Functions to calculate e.g. the migration traffic rate (MTR) by
vertically integrating profiles (`vp` or `vpts`) into an integrated
profile (`vpi`).

- [`integrate_profile()`](http://adriaandokter.com/bioRad/dev/reference/integrate_profile.md)
  :

  Vertically integrate profiles (`vp` or `vpts`) into an integrated
  profile (`vpi`)

- [`plot(`*`<vpi>`*`)`](http://adriaandokter.com/bioRad/dev/reference/plot.vpi.md)
  :

  Plot an integrated profile (`vpi`)

- [`summary(`*`<vpi>`*`)`](http://adriaandokter.com/bioRad/dev/reference/summary.vpi.md)
  [`is.vpi()`](http://adriaandokter.com/bioRad/dev/reference/summary.vpi.md)
  :

  Inspect an integrated profile (`vpi`)

## Accessing vertical profile metadata

Functions to access metadata of vertical profiles (`vp`), time series
(`vpts`) or integrated profiles (`vpi`).

- [`attribute_table()`](http://adriaandokter.com/bioRad/dev/reference/attribute_table.md)
  : Extract a volume coverage pattern table with all attributes
- [`check_night()`](http://adriaandokter.com/bioRad/dev/reference/check_night.md)
  : Check if it is night at a given time and place
- [`doy()`](http://adriaandokter.com/bioRad/dev/reference/doy_noy.md)
  [`noy()`](http://adriaandokter.com/bioRad/dev/reference/doy_noy.md) :
  Look up day of year (doy) or night of year (noy)
- [`rcs()`](http://adriaandokter.com/bioRad/dev/reference/rcs.md) : Get
  radar cross section
- [`sd_vvp_threshold()`](http://adriaandokter.com/bioRad/dev/reference/sd_vvp_threshold.md)
  : Get threshold of the radial velocity standard deviation

## Radar beam geometry

Functions relating the radar beam shape to range, distance and height.

- [`beam_distance()`](http://adriaandokter.com/bioRad/dev/reference/beam_distance.md)
  : Calculate radar beam distance
- [`beam_height()`](http://adriaandokter.com/bioRad/dev/reference/beam_height.md)
  : Calculate radar beam height
- [`beam_profile()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile.md)
  : Calculate vertical radiation profile
- [`beam_profile_overlap()`](http://adriaandokter.com/bioRad/dev/reference/beam_profile_overlap.md)
  : Calculate overlap between a vertical profile ('vp') and the vertical
  radiation profile emitted by the radar
- [`beam_range()`](http://adriaandokter.com/bioRad/dev/reference/beam_range.md)
  : Calculate radar beam range
- [`beam_width()`](http://adriaandokter.com/bioRad/dev/reference/beam_width.md)
  : Calculate radar beam width

## Other functions

Other useful functions.

- [`convert_legacy()`](http://adriaandokter.com/bioRad/dev/reference/convert_legacy.md)
  : Convert legacy bioRad objects
- [`dbz_to_eta()`](http://adriaandokter.com/bioRad/dev/reference/dbz_to_eta.md)
  : Convert reflectivity factor (dBZ) to reflectivity (eta)
- [`eta_to_dbz()`](http://adriaandokter.com/bioRad/dev/reference/eta_to_dbz.md)
  : Convert reflectivity (eta) to reflectivity factor (dBZ)
- [`nyquist_velocity()`](http://adriaandokter.com/bioRad/dev/reference/nyquist_velocity.md)
  : Calculate Nyquist velocity for a given pulse repetition frequency
  (PRF)
- [`sunrise()`](http://adriaandokter.com/bioRad/dev/reference/sunrise_sunset.md)
  [`sunset()`](http://adriaandokter.com/bioRad/dev/reference/sunrise_sunset.md)
  : Calculate sunrise or sunset for a time and place
