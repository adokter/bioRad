# Calculate a vertical profile (`vp`) from a polar volume (`pvol`) file

Calculates a vertical profile of biological scatterers (`vp`) from a
polar volume (`pvol`) file using the algorithm
[vol2bird](https://github.com/adokter/vol2bird/) (Dokter et al. 2011
[doi:10.1098/rsif.2010.0116](https://doi.org/10.1098/rsif.2010.0116) ).

## Usage

``` r
calculate_vp(
  file,
  vpfile = "",
  pvolfile_out = "",
  autoconf = FALSE,
  verbose = FALSE,
  warnings = TRUE,
  sd_vvp_threshold,
  rcs = 11,
  dual_pol = TRUE,
  rho_hv = 0.95,
  single_pol = TRUE,
  elev_min = 0,
  elev_max = 90,
  azim_min = 0,
  azim_max = 360,
  range_min = 5000,
  range_max = 35000,
  n_layer = 20,
  h_layer = 200,
  height_reference = "sea",
  ground_height_param = "HGHT",
  dealias = TRUE,
  nyquist_min = if (dealias) 5 else 25,
  nyquist_max_dealias = 25,
  dbz_quantity = "DBZH",
  eta_max = 36000,
  mistnet = FALSE,
  mistnet_elevations = c(0.5, 1.5, 2.5, 3.5, 4.5),
  local_mistnet
)
```

## Arguments

- file:

  Character (vector). Either a path to a single radar polar volume
  (`pvol`) file containing multiple scans/sweeps, or multiple paths to
  scan files containing a single scan/sweep. Or a single `pvol` object.
  The file data format should be either 1)
  [ODIM](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf)
  format, which is the implementation of the OPERA data information
  model in the [HDF5](https://www.hdfgroup.org/solutions/hdf5/)
  format, 2) a format supported by the [RSL
  library](https://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/) or 3)
  Vaisala IRIS (IRIS RAW) format.

- vpfile:

  Character. File name. When provided, writes a vertical profile file
  (`vpfile`) either in the VPTS CSV or ODIM HDF5 format to disk.

- pvolfile_out:

  Character. File name. When provided, writes a polar volume (`pvol`)
  file in the ODIM HDF5 format to disk. Useful for converting RSL
  formats to ODIM.

- autoconf:

  Logical. When `TRUE`, default optimal configuration settings are
  selected automatically and other user settings are ignored.

- verbose:

  Logical. When `TRUE`, vol2bird `stdout` is piped to the R console.

- warnings:

  Logical. When `TRUE`, vol2bird warnings are piped to the R console.

- sd_vvp_threshold:

  Numeric. Lower threshold for the radial velocity standard deviation
  (profile quantity `sd_vvp`) in m/s. Biological signals with
  `sd_vvp < sd_vvp_threshold` are set to zero. Defaults to 2 m/s for
  C-band radars and 1 m/s for S-band radars.

- rcs:

  Numeric. Radar cross section per bird to use, in cm^2.

- dual_pol:

  Logical. When `TRUE`, uses dual-pol mode, in which meteorological
  echoes are filtered using the correlation coefficient threshold
  `rho_hv`.

- rho_hv:

  Numeric. Lower threshold in correlation coefficient to use for
  filtering meteorological scattering.

- single_pol:

  Logical. When `TRUE`, uses precipitation filtering in single
  polarization mode based on reflectivity and radial velocity
  quantities.

- elev_min:

  Numeric. Minimum elevation angle to include, in degrees.

- elev_max:

  Numeric. Maximum elevation angle to include, in degrees.

- azim_min:

  Numeric. Minimum azimuth to include, in degrees clockwise from north.

- azim_max:

  Numeric. Maximum azimuth to include, in degrees clockwise from north.

- range_min:

  Numeric. Minimum range to include, in m.

- range_max:

  Numeric. Maximum range to include, in m.

- n_layer:

  Numeric. Number of altitude layers to use in generated profile.

- h_layer:

  Numeric. Width of altitude layers to use in generated profile, in m.

- height_reference:

  Character. One of `sea`, `antenna` or `ground` for specifying the
  reference height for the profile altitude bins. Default `sea` level.

- ground_height_param:

  Character. The scan parameter name of the polar volume containing
  ground height information. Default `HGHT`.

- dealias:

  Logical. Whether to dealias radial velocities. This should typically
  be done when the scans in the polar volume have low Nyquist velocities
  (below 25 m/s).

- nyquist_min:

  Numeric. Minimum Nyquist velocity of scans to include, in m/s.

- nyquist_max_dealias:

  Numeric. When all scans have nyquist velocity higher than this value,
  dealiasing is suppressed. Default 25 m/s.

- dbz_quantity:

  Name of the available reflectivity factor to use if not `DBZH` (e.g.
  `DBZV`, `TH`, `TV`).

- eta_max:

  Maximum reflectivity in cm^2/km^3 for single gates containing birds.
  Default 36000 cm^2/km^3, corresponding to approximately 20 dBZ at
  C-band and 32 dBZ at S-band. Gates with reflectivities above this
  threshold will be discarded prior to profile estimation.

- mistnet:

  Logical. Whether to use the MistNet segmentation model.

- mistnet_elevations:

  Numeric vector of length 5. Elevation angles to feed to the MistNet
  segmentation model, which expects exactly 5 elevation scans at 0.5,
  1.5, 2.5, 3.5 and 4.5 degrees. Specifying different elevation angles
  may compromise segmentation results.

- local_mistnet:

  Character. Path to local MistNet segmentation model in PyTorch format
  (e.g. `/your/path/mistnet_nexrad.pt`).

## Value

A vertical profile object of class `vp`. When defined, output files
`vpfile` and `pvolfile_out` are saved to disk.

## Details

### Typical use

Common arguments set by users are `file`, `vpfile` and `autoconf`. Turn
on `autoconf` to automatically select the optimal parameters for a given
radar file. The default for C-band data is to apply rain-filtering in
single polarization mode and dual polarization mode when available. The
default for S-band data is to apply precipitation filtering in
dual-polarization mode only.

Arguments that sometimes require non-default values are: `rcs`,
`sd_vvp_threshold`, `range_max`, `dual_pol`, `dealias`. Other arguments
are typically left at their defaults.

### sd_vvp_threshold

For altitude layers with a VVP-retrieved radial velocity standard
deviation value below the threshold `sd_vvp_threshold`, the bird density
`dens` is set to zero (see vertical profile
[`vp`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)
class). This threshold might be dependent on radar processing settings.
Results from validation campaigns so far indicate that 2 m/s is the best
choice for this parameter for most C-band weather radars, which is used
as the C-band default. For S-band, the default threshold is 1 m/s.

### rcs

The default radar cross section (`rcs`) (11 cm^2) corresponds to the
average value found by Dokter et al. (2011) in a calibration campaign of
a full migration autumn season in western Europe at C-band. Its value
may depend on radar wavelength. `rcs` will scale approximately
\\M^{2/3}\\ with `M` the bird's mass.

### dual_pol

For S-band (radar wavelength ~ 10 cm), currently only `dual_pol = TRUE`
mode is recommended.

### azim_min / azim_max

`azim_min` and `azim_max` only affects reflectivity-derived estimates in
the profile (`DBZH`, `eta`, `dens`), not radial-velocity derived
estimates (`u`, `v`, `w`, `ff`, `dd`, `sd_vvp`), which are estimated on
all azimuths at all times. `azim_min`, `azim_max` may be set to exclude
an angular sector with high ground clutter.

### range_min / range_max

Using default values of `range_min` and `range_max` is recommended.
Ranges closer than 5 km tend to be contaminated by ground clutter, while
range gates beyond 35 km become too wide to resolve the default altitude
layer width of 200 meter (see
[`beam_width()`](http://adriaandokter.com/bioRad/dev/reference/beam_width.md)).
`range_max` may be extended up to 40 km (`40000`) for volumes with low
elevations only, in order to extend coverage to higher altitudes.

### h_layer

The algorithm has been tested and developed for altitude layers with
`h_layer = 200`m. Smaller widths than 100 m are not recommended as they
may cause instabilities of the volume velocity profiling (VVP) and
dealiasing routines, and effectively lead to pseudo-replicated altitude
data, since altitudinal patterns smaller than the beam width cannot be
resolved.

### dealias

Dealiasing uses the torus mapping method by Haase et al. (2004).

Use parameter `nyquist_min` to discard specific elevation scans with
very low Nyquist velocity (by default smaller than 5 m/s). The Haase et
al. algorithm is known to not provide accurate estimates for heavily
folded velocity data, therefore these elevation scans are best removed
entirely.

Use parameter `nyquist_max_dealias` to suppress dealiasing for polar
volumes for which all scans already have a high Nyquist velocity. This
prevents dealiasing when the data does not require it.

### height reference

Profiles are calculated by default for height bins defined relative to
mean sea level. Alternatively, height bins may be defined relative to
the radar antenna height by setting `height_reference` to `antenna`.
This places the bottom of the lowest altitude bin at the radar antenna
height. Profiles may also be calculated relative to the height of the
ground level terrain. This is especiallyuseful for stopover studies
focused on altitude distributions during peak exodus shortly after
sunset. Estimating a profile relative to ground height requires adding
information from a digital elevation map to each pixel of the input
polar volume, which is accomplished easily with function
[`add_param()`](http://adriaandokter.com/bioRad/dev/reference/add_param.md).
Ground heights should be stored in units of meters relative to mean sea
level. Parameter `ground_height_param` should point to the scan
parameter name containing the digital elevation information.

### Local installation

You may point parameter `local_mistnet` to a local download of the
MistNet segmentation model in PyTorch format, e.g.
`/your/path/mistnet_nexrad.pt`. The MistNet model can be downloaded at
<https://s3.amazonaws.com/mistnet/mistnet_nexrad.pt>.

## References

Dokter et al. (2011) is the main reference for the profiling algorithm
(vol2bird) underlying this function. When using the `mistnet` option,
please also cite Lin et al. (2019). When dealiasing data (`dealias`),
please also cite Haase et al. (2004).

- Dokter AM, Liechti F, Stark H, Delobbe L,Tabary P, Holleman I (2011)
  Bird migration flight altitudes studied by a network of operational
  weather radars, Journal of the Royal Society Interface 8 (54), pp.
  30-43.
  [doi:10.1098/rsif.2010.0116](https://doi.org/10.1098/rsif.2010.0116)

- Haase G & Landelius T (2004) Dealiasing of Doppler radar velocities
  using a torus mapping. Journal of Atmospheric and Oceanic Technology
  21(10), pp. 1566-1573.
  [doi:10.1175/1520-0426(2004)021\<1566:DODRVU\>2.0.CO;2](https://doi.org/10.1175/1520-0426%282004%29021%3C1566%3ADODRVU%3E2.0.CO%3B2)

- Lin T-Y, Winner K, Bernstein G, Mittal A, Dokter AM, Horton KG,
  Nilsson C, Van Doren BM, Farnsworth A, La Sorte FA, Maji S, Sheldon
  D (2019) MistNet: Measuring historical bird migration in the US using
  archived weather radar data and convolutional neural networks. Methods
  in Ecology and Evolution 10 (11), pp. 1908-22.
  [doi:10.1111/2041-210X.13280](https://doi.org/10.1111/2041-210X.13280)

## See also

- [`summary.pvol()`](http://adriaandokter.com/bioRad/dev/reference/summary.pvol.md)

- [`summary.vp()`](http://adriaandokter.com/bioRad/dev/reference/summary.vp.md)

- [`integrate_to_ppi()`](http://adriaandokter.com/bioRad/dev/reference/integrate_to_ppi.md)

- [`add_param()`](http://adriaandokter.com/bioRad/dev/reference/add_param.md)

## Examples

``` r
# Locate and read the polar volume example file
pvolfile_source <- system.file("extdata", "volume.h5", package = "bioRad")

# Copy the file to a temporary directory with read/write permissions
pvolfile <- paste0(tempdir(),"/volume.h5")
file.copy(pvolfile_source, pvolfile)
#> [1] TRUE

# Calculate the profile
if (requireNamespace("vol2birdR", quietly = TRUE)) {
vp <- calculate_vp(pvolfile)

# Get summary info
vp

# By default profiles are calculated for bins defined relative to sea level
# To calculate a profile relative to ground level:
# \donttest{
if(requireNamespace("elevatr", quietly = TRUE)){

example_pvol <- read_pvolfile(pvolfile)

# Download digital elevation model (DEM) information:
example_pvol |>
  # extract lowest scan
  get_scan(.5) |>
  # convert to raster object
  scan_to_raster(param="DBZH") |>
  # convert to terra raster class
  terra::rast() |>
  # download digital elevation data (increase z for higher resolutions)
  elevatr::get_elev_raster(z = 5, clip = "bbox") -> data_dem
# set digital elevations for open water to mean sea level (0)
data_dem[data_dem<0]=0
# set an informative name for the DEM information
names(data_dem) <- "HGHT"

# add the DEM information as a scan parameter to the polar volume:
example_pvol <- add_param(example_pvol, data_dem, "HGHT")

# calculate a profile relative to ground level:
vp_ground <- calculate_vp(example_pvol, height_reference="ground", ground_height_param="HGHT")

}
# }

# Clean up
file.remove(pvolfile)

}
#> Running vol2birdSetUp
#> Warning: radial velocities will be dealiased...
#> Mosaicing & Projecting
#> Clipping DEM to bbox
#> Note: Elevation units are in meters.
#> Running vol2birdSetUp
#> Warning: radial velocities will be dealiased...
#> [1] TRUE
```
