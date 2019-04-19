# environment for storing dynamic package flags
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  has_docker <- (check_docker(verbose = FALSE) == 0)
  # flag indicating whether docker is running:
  .pkgenv[["docker"]] <- has_docker
  # flag indicating whether vol2bird docker container is mounted:
  .pkgenv[["mounted"]] <- FALSE
  # the current mountpoint of the vol2bird docker container:
  .pkgenv[["mount"]] <- "~/"
  if(has_docker){
    .pkgenv[["vol2bird_version"]] <- vol2bird_version()
  }
  else{
    .pkgenv[["vol2bird_version"]] <- NA
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Welcome to", pkgname, "version", packageVersion(pkgname)))
  if (!.pkgenv$docker) {
    msg <- paste(
      "Warning: no running Docker daemon found\n",
      "Warning:", pkgname, "functionality requiring Docker has been disabled\n\n",
      "To enable Docker functionality,",
      "start Docker and run 'check_docker()' in R"
    )
    msg <- paste(strwrap(msg), collapse = "\n")
    packageStartupMessage(msg)
  } else {
    packageStartupMessage(paste("Docker daemon running, Docker functionality enabled (vol2bird version",.pkgenv$vol2bird_version,")",sep=""))
  }
}
