# environment for storing dynamic package flags
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
  # latest available vol2bird release
  .pkgenv[["latest_vol2bird_version"]] <- numeric_version("0.5.0")
  # availability of mistnet
  .pkgenv[["mistnet"]] <- vol2birdR::mistnet_exists()
  # attempt to determine available vol2bird version
  .pkgenv[["vol2bird_version"]] <- vol2birdR::vol2bird_version()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to", pkgname, "version", packageVersion(pkgname)))
  packageStartupMessage(paste(ifelse(is.null(.pkgenv$vol2bird_version), "", paste("(vol2bird version ", .pkgenv$vol2bird_version, ifelse(.pkgenv$mistnet, ", MistNet available", ""), ")", sep = ""))))
}
