.onLoad <- function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to", pkgname, "version", packageVersion(pkgname)))
  packageStartupMessage(paste("(vol2bird version ", vol2birdR::vol2bird_version(), ifelse(vol2birdR::mistnet_exists(), ", MistNet available", ""), ")", sep = ""))
}
