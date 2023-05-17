.onLoad <- function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to", pkgname, "version", packageVersion(pkgname)))
  if (rlang::is_installed("vol2birdR")) {
    packageStartupMessage(paste("using vol2birdR version ", packageVersion("vol2birdR"), ifelse(vol2birdR::mistnet_exists(), " (MistNet installed)", " (MistNet not installed)"), sep = ""))
  }
}
