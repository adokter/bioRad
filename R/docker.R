#' Check if Docker is running
#'
#' Checks that \href{https://www.docker.com/}{Docker} daemon is running
#' correctly on the local system
#' @param verbose logical. When TRUE test results are printed to
#' R console (On Windows positive test results are always printed),
#' when FALSE a numeric error-code is returned.
#' @export
#' @return If \code{verbose} is False: 0 upon success, otherwise an error code.
#' If \code{verbose} is True, no value is returned.
check_docker <- function(verbose = TRUE) {
  docker_command_rm = "docker rm -f hello-world"
  docker_command_run = "docker run --rm --name hello-world hello-world"
  if (.Platform$OS.type == "unix") {
    suppressWarnings(
      system(docker_command_rm,
        ignore.stderr = TRUE,
        ignore.stdout = TRUE
      )
    )
    result <- suppressWarnings(
        system(docker_command_run,
        ignore.stderr = !verbose, ignore.stdout = !verbose
      )
    )
  } else {
    suppressWarnings(
      system(docker_command_rm,
        ignore.stderr = TRUE,
        ignore.stdout = TRUE, show.output.on.console = FALSE
      )
    )
    result <- suppressWarnings(
      system(docker_command_run,
        ignore.stderr = !verbose, ignore.stdout = !verbose,
        show.output.on.console = TRUE
      )
    )
  }
  .pkgenv$docker <- (result == 0)
  .pkgenv$mounted <- FALSE
  if (!verbose) {
    return(result)
  }
  else{
    if(result!=0) cat("Docker check failed: exit code", result)
  }
}

#' Update Docker image from Docker hub
#'
#' Pulls and installs the latest Docker image used by bioRad from Docker hub
#' @details
#' This command pulls the latest
#' \href{https://hub.docker.com/r/adokter/vol2bird/}{vol2bird} Docker image
#' from \href{https://hub.docker.com}{Docker hub}.
#' Run this command to ensure all Docker functionality (e.g. the
#' \link{vol2bird} function) runs at the latest available version.
#' @export
#' @return the POSIXct creation date of the installed Docker image
update_docker <- function() {
  creationDate <- NULL

  result <- suppressWarnings(system("docker pull adokter/vol2bird:latest"))
  if (result == 0) {
    creationDate <- system(
      "docker inspect -f \"{{ .Created }}\" adokter/vol2bird:latest",
      intern = TRUE
    )
    # initialize new container.
    .pkgenv$vol2bird_version <- vol2bird_version()
    .pkgenv$docker <- !is.na(.pkgenv$vol2bird_version)
    .pkgenv$mounted <- FALSE
    if(is.na(.pkgenv$vol2bird_version)){
      stop("Failed to initialize newly pulled Docker image")
    }
    else{
      print(.pkgenv$vol2bird_version)
      cat("Succesfully installed Docker image with vol2bird version ",.pkgenv$vol2bird_version)
    }

  }
  else{
    stop("Failed to pull Docker image")
  }

  if (!is.null(creationDate)) {
    # docker reports time stamps in Zulu (UTC) time
    creationDate <- as.POSIXct(creationDate, format = "%Y-%m-%dT%T", tz = "UTC")
  }

  return(creationDate)
}

mount_docker_container <- function(mount = "~/") {
  # if docker not running, cannot start container
  if (!.pkgenv$docker) {
    return(1)
  }
  # if container already running at this mount point, nothing to be done:
  if (.pkgenv$mounted & .pkgenv$mount == mount) {
    return(0)
  }
  # remove any existing vol2bird containers
  if (.Platform$OS.type == "unix") {
    system("docker rm -f vol2bird", ignore.stderr = TRUE, ignore.stdout = TRUE)
  } else {
    suppressWarnings(system("docker rm -f vol2bird",
      ignore.stderr = TRUE, ignore.stdout = TRUE,
      show.output.on.console = FALSE
    ))
  }
  # fire up the container:
  if (.Platform$OS.type == "unix") {
    result <- system(
      paste("docker run -v ",
        system(paste("printf %q ", shQuote(normalizePath(mount)), sep = ""), intern = T),
        ":/data -d --name vol2bird adokter/vol2bird sleep infinity",
        sep = ""
      ),
      ignore.stdout = TRUE
    )
  } else {
    result <- suppressWarnings(system(
      paste("docker run -v ",
        shQuote(normalizePath(mount, winslash = "/")),
        ":/data -d --name vol2bird adokter/vol2bird sleep infinity",
        sep = ""
      ),
      ignore.stdout = TRUE, show.output.on.console = FALSE
    ))
  }
  if (result != 0) {
    warning(paste(
      "failed to mount", mount, "... Go to 'Docker -> preferences",
      "-> File Sharing' and add this directory (or its root",
      "directory) as a bind mounted directory"
    ))
  } else {
    .pkgenv$mounted <- (result == 0)
    .pkgenv$mount <- mount
  }
  return(result)
}

#' Check version of the vol2bird algorithm used by bioRad
#'
#' Checks that \href{https://www.docker.com/}{Docker} daemon is running
#' correctly on the local system and returns the version of the
#' installed vol2bird algorithm in the Docker container.
#'
#' @details when argument \code{vol2bird_local_install} is specified with
#' a path to a local executable of vol2bird, the function will return
#' the version of this local installation.
#'
#' @export
#' @param vol2bird_local_install (optional) String with path to local
#' vol2bird installation, see \link{calculate_vp} for details.
#' @return an object of class \link{numeric_version}
vol2bird_version <- function(vol2bird_local_install) {

  creationDate <- NA

  if(!missing(vol2bird_local_install)){
    vol2bird_version=suppressWarnings(system(paste("bash -l -c \"",vol2bird_local_install,"--version\""),intern=T))
    vol2bird_version <- strsplit(trimws(vol2bird_version),split=" ")[[1]][3]
    return(numeric_version(vol2bird_version))
  }

  creationDate <- suppressWarnings(try(system(
    "docker inspect -f \"{{ .Created }}\" adokter/vol2bird",
    intern = TRUE, ignore.stderr = TRUE
  ),silent=TRUE))

  # return NA if docker command failed.
  if(class(creationDate)=="try-error") return(NA)

  # this occurs when there is no adokter/vol2bird container, resulting in an error.
  if(length(creationDate)>1) creationDate=creationDate[1]

  # docker reports time stamps in Zulu (UTC) time
  creationDate <- as.POSIXct(creationDate, format = "%Y-%m-%dT%T", tz = "UTC")

  # return NA if no valid time stamp found
  if(is.na(creationDate)) return(NA)

  if(as.numeric(format(creationDate,"%Y"))<2019){
    # vol2bird version generated before 2019, i.e. version < 0.4.0
    vol2bird_version <- suppressWarnings(system(
      "docker run --rm adokter/vol2bird bash -c \"vol2bird 2>&1 | grep Version\"",
      intern = TRUE
    ))
    vol2bird_version <- strsplit(trimws(vol2bird_version),split=" ")[[1]][2]
  }
  else{
    # vol2bird version >= 0.4.0, supporting --version argument
    vol2bird_version <- suppressWarnings(system(
      "docker run --rm adokter/vol2bird bash -c \"vol2bird --version\"",
      intern = TRUE
    ))
    vol2bird_version <- strsplit(trimws(vol2bird_version),split=" ")[[1]][3]
  }
  return(numeric_version(vol2bird_version))
}
