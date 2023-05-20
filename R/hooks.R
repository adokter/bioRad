.onLoad <- function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
  assertthat::assert_that(assertthat::is.string(find.package("vol2birdR", verbose=F, quiet=T)),msg="package vol2birdR not found. See https://adokter.github.io/vol2birdR/ for install instructions")
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to", pkgname, "version", utils::packageVersion(pkgname)))
  packageStartupMessage(paste("using vol2birdR version ", utils::packageVersion("vol2birdR"), ifelse(vol2birdR::mistnet_exists(), " (MistNet installed)", " (MistNet not installed)"), sep = ""))
}
