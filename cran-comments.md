## bioRad 0.8.0 

Addresses CRAN policy violation of copying files to user home directory in examples

Features:
1. VPTS files are now able to be validated with `validate_vpts()`, enabled by a pre-defined VPTS schema `vpts_schema.rda`, now included in the package data

Fixes:
1. Handle empty numeric vectors when plotting clutter maps
2. `download_vpfiles()` points to a new aloft S3 bucket
3. Default refractive index of water changed in `eta_to_dbz()` and `dbz_to_eta()`
4. Handle VPTS files with multiple sd_vvp_thresholds