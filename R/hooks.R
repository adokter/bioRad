.onLoad <- function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
  if (!"vol2birdR" %in% utils::installed.packages()) {
    warning("Package 'vol2birdR' is not found. Please see https://adokter.github.io/vol2birdR/ for installation instructions.")
  }
}
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to", pkgname, "version", utils::packageVersion(pkgname)))
  if (rlang::is_installed("vol2birdR")) {
    packageStartupMessage(paste("using vol2birdR version ", utils::packageVersion("vol2birdR"), ifelse(vol2birdR::mistnet_exists(), " (MistNet installed)", " (MistNet not installed)"), sep = ""))
  }
  if (requireNamespace("sp", quietly = TRUE)) {
    sp::set_evolution_status(2L)
    packageStartupMessage("Assigning sp_evolution_status to 2. See sp::get_evolution_status()")
    packageStartupMessage("This is required until the 'sp' package deprecates 'rgdal'")
  }
}
