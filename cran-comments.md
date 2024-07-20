## bioRad 0.8.0 

* Addresses CRAN policy violation of copying files to user home directory in examples
* Make checks pass without suggested packages, i.e. when _R_CHECK_DEPENDS_ONLY_=true

New Features:
1. VPTS files are now able to be validated with `validate_vpts()`, enabled by a pre-defined VPTS schema `vpts_schema.rda`, now included in the package data
2. Speed up `integrate_to_ppi()` and `project_as_ppi()` by using native `sf` functions
3. Added support for tidyverse select method for polar volume and polar scan objects

Additional Fixes:
1. Handle empty numeric vectors when plotting clutter maps
2. `download_vpfiles()` points to a new aloft S3 bucket
3. Default refractive index of water changed in `eta_to_dbz()` and `dbz_to_eta()`
4. Handle VPTS files with multiple sd_vvp_thresholds
5. Fixed enabling/disabling of `single_pol` flag in `calculate_vp()
