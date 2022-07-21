#' Check version of the vol2bird algorithm used by bioRad
#'
#' @details when argument \code{local_install} is specified with
#' a path to a local executable of vol2bird, the function will return
#' the version of this local installation.
#'
#' @export
#' @param local_install (optional) String with path to local
#' vol2bird installation, see \link{calculate_vp} for details.
#' @return an object of class \link{numeric_version}
#'
#' @examples
#' # check installed vol2bird version:
#' vol2bird_version()
vol2bird_version <- function(local_install) {
  if (!missing(local_install)) {
    vol2bird_version <- suppressWarnings(system(paste("bash -l -c \"", local_install, "--version\""), intern = T))
    vol2bird_version <- strsplit(trimws(vol2bird_version), split = " ")[[1]][3]
    return(numeric_version(vol2bird_version))
  }

  if(vol2birdR::mistnet_exists()){
    .pkgenv$mistnet <- TRUE
  }

  return(vol2birdR::vol2bird_version())
}
