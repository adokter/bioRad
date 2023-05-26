.onLoad <- function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
  assertthat::assert_that(assertthat::is.string(find.package("vol2birdR", verbose=F, quiet=T)),msg="package vol2birdR not found. See https://adokter.github.io/vol2birdR/ for install instructions")
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
