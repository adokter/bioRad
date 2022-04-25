# environment for storing dynamic package flags
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  register_all_s3_methods() # dynamically registers non-imported pkgs (tidyverse) # nocov
  # latest available vol2bird release
  .pkgenv[["latest_vol2bird_version"]] <- numeric_version("0.5.0")
  # availability of mistnet
  .pkgenv[["mistnet"]] <- FALSE
  # attempt to determine available vol2bird version
  .pkgenv[["vol2bird_version"]] <- vol2bird_version()
  # flag indicating whether docker is running:
  .pkgenv[["docker"]] <- ifelse(is.null(.pkgenv[["vol2bird_version"]]), TRUE, !is.na(.pkgenv[["vol2bird_version"]]))
  # flag indicating whether vol2bird docker container is mounted:
  .pkgenv[["mounted"]] <- FALSE
  # the current mountpoint of the vol2bird docker container:
  .pkgenv[["mount"]] <- "~/"
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to", pkgname, "version", packageVersion(pkgname)))
  if (!.pkgenv$docker) {
    if (is.nan(.pkgenv[["vol2bird_version"]])) {
      packageStartupMessage("Warning: Docker daemon is not running")
      msg_action <- "start Docker and run 'check_docker()' in R"
    }
    else {
      # vol2bird_version equals NA:
      packageStartupMessage("Warning: Docker daemon not found")
      msg_action <- "first install Docker"
    }
    msg <- paste(
      "Warning:", pkgname, "functionality requiring Docker has been disabled\n\n",
      "To enable Docker functionality,", msg_action
    )
    msg <- paste(strwrap(msg), collapse = "\n")
    packageStartupMessage(msg)
  } else {
    packageStartupMessage(paste("Docker daemon running, Docker functionality enabled", ifelse(is.null(.pkgenv$vol2bird_version), "", paste("(vol2bird version ", .pkgenv$vol2bird_version, ifelse(.pkgenv$mistnet, ", MistNet available", ""), ")", sep = ""))))
    if (is.null(.pkgenv[["vol2bird_version"]])) {
      packageStartupMessage(paste("No vol2bird Docker image found. Please run update_docker() to download."))
    }
    else {
      if (.pkgenv[["vol2bird_version"]] < .pkgenv[["latest_vol2bird_version"]]) {
        packageStartupMessage("Your Docker image contains an obsolete version of vol2bird. Consider updating with update_docker()")
      }
    }
  }
}
