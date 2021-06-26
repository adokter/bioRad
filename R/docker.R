#' Check if Docker is running
#'
#' Checks if the [Docker](https://www.docker.com/) daemon is running correctly
#' on the local system and that vol2bird Docker image is available.
#'
#' @param verbose Logical. When `TRUE`, messages are printed to R console.
#'
#' @return
#' * `0`: Success.
#' * `1`: Error: Docker vol2bird image not available.
#' * `2`: Error: Docker daemon not running.
#' * `3`: Error: Docker daemon not found.
#'
#' @export
#'
#' @seealso
#' * [update_docker()]
#'
#' @examples
#' \dontrun{
#' # Check if Docker is running and vol2bird image is available
#' check_docker()
#' }
check_docker <- function(verbose = TRUE) {
  check <- vol2bird_version() # note: the vol2bird_version() call also sets .pkgenv$mistnet

  .pkgenv$vol2bird_version <- check
  .pkgenv$docker <- !is.na(check)
  .pkgenv$mounted <- FALSE

  if (is.null(check)) {
    if (verbose) warning("vol2bird docker container not downloaded, run update_docker()")
    return(1)
  }

  if (class(check) != "numeric_version") {
    if (is.nan(check)) {
      if (verbose) warning("Docker daemon not running, please start Docker")
      return(2)
    }

    if (is.na(check)) {
      if (verbose) warning("Docker daemon not found")
      return(3)
    }
  }

  if (verbose) cat(paste("Running docker image with vol2bird version", check, ifelse(.pkgenv$mistnet, " (MistNet available)", ""), "\n"))
  return(0)
}

#' Update Docker image from Docker hub
#'
#' Pulls and installs the latest [vol2bird Docker
#' image](https://hub.docker.com/r/adokter/vol2bird/) from Docker hub. Run this
#' command to ensure all Docker functionality (e.g. for [calculate_vp()]) makes
#' use of the latest available version.
#'
#' @param mistnet Logical. When `TRUE`, installs the MistNet segmentation model,
#'   downloading an additional 1GB Docker image (see [apply_mistnet()]).
#'
#' @return The POSIXct creation date of the installed Docker image.
#'
#' @export
#'
#' @seealso
#' * [check_docker()]
#'
#' @examples
#' \dontrun{
#' # Update the vol2bird docker image
#' update_docker()
#'
#' # Update the vol2bird docker image and install the MistNet segmentation model
#' update_docker(mistnet = TRUE)
#' }
update_docker <- function(mistnet = FALSE) {
  creationDate <- NULL

  if (suppressWarnings(system("docker", ignore.stderr = T, ignore.stdout = T)) != 0) stop("Docker daemon not found")

  image_id <- docker_image_id("adokter/vol2bird")
  result <- suppressWarnings(system("docker pull adokter/vol2bird:latest"))
  image_id_new <- docker_image_id("adokter/vol2bird")

  if (result == 0 && !mistnet && !identical(image_id, image_id_new)) {
    # a new image has been installed, which makes the installed
    # mistnet image obsolete. Therefore remove if existing
    image_id_mistnet <- docker_image_id("adokter/vol2bird-mistnet")
    if (length(image_id_mistnet) > 0) {
      system("docker rmi -f adokter/vol2bird-mistnet:latest")
      warning("Obsolete MistNet installation removed. To use MistNet, reinstall with update_docker(mistnet=TRUE)")
    }
  }

  if (result == 0 && mistnet) result <- suppressWarnings(system("docker pull adokter/vol2bird-mistnet:latest"))

  if (result == 0) {

    # clean up dangling images
    system("docker image prune -f")

    creationDate <- system(
      paste("docker inspect -f \"{{ .Created }}\" adokter/vol2bird", ifelse(mistnet, "-mistnet", ""), ":latest", sep = ""),
      intern = TRUE
    )
    # initialize new container.
    .pkgenv$vol2bird_version <- vol2bird_version()
    .pkgenv$docker <- !is.na(.pkgenv$vol2bird_version)
    .pkgenv$mounted <- FALSE
    if (is.na(.pkgenv$vol2bird_version)) {
      stop("Failed to initialize newly pulled Docker image")
    }
    else {
      cat(paste("Succesfully installed Docker image with vol2bird version", .pkgenv$vol2bird_version, ifelse(.pkgenv$mistnet, ", including MistNet.", ""), "\n"))
    }
  }
  else {
    stop("Failed to pull Docker image")
  }

  if (!is.null(creationDate)) {
    # docker reports time stamps in Zulu (UTC) time
    creationDate <- as.POSIXct(creationDate, format = "%Y-%m-%dT%T", tz = "UTC")
  }

  return(creationDate)
}

docker_image_id <- function(image_name) {
  docker_image <- system(paste("docker images ", image_name, ":latest", sep = ""), intern = T)
  image_id <- read.table(textConnection(docker_image), stringsAsFactors = F, header = T)$TAG
  return(image_id)
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
        system(paste("/bin/bash -c", shQuote(paste("printf %q", shQuote(normalizePath(mount))))), intern = T),
        ":/data -d --name vol2bird adokter/vol2bird", ifelse(.pkgenv$mistnet, "-mistnet", ""), " sleep infinity",
        sep = ""
      ),
      ignore.stdout = TRUE
    )
  } else {
    result <- suppressWarnings(system(
      paste("docker run -v ",
        shQuote(normalizePath(mount, winslash = "/")),
        ":/data -d --name vol2bird adokter/vol2bird", ifelse(.pkgenv$mistnet, "-mistnet", ""), " sleep infinity",
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

#' Get installed vol2bird version
#'
#' Returns the version of the vol2bird algorithm currently installed and used by
#' bioRad. This will either be the version installed in the Docker daemon
#' (default, see [check_docker()]) or the version at `local_install`.
#'
#' @param local_install Character. Path to local vol2bird installation, see
#'   [calculate_vp()] for details.
#'
#' @return
#' * Version number of installed vol2bird.
#' * `NA`: Error: Docker system command not available.
#' * `NaN`: Error: Docker daemon not running.
#' * `NULL`: Error: Docker vol2bird image not available.
#'
#' @export
#'
#' @seealso
#' * [check_docker()]
#' * [update_docker()]
#'
#' @examples
#' \dontrun{
#' # Check installed vol2bird version
#' vol2bird_version()
#' }
vol2bird_version <- function(local_install) {
  creationDate <- NA

  if (!missing(local_install)) {
    vol2bird_version <- suppressWarnings(system(paste("bash -l -c \"", local_install, "--version\""), intern = T))
    vol2bird_version <- strsplit(trimws(vol2bird_version), split = " ")[[1]][3]
    return(numeric_version(vol2bird_version))
  }

  imagePresent <- suppressWarnings(try(system(paste("docker images -q adokter/vol2bird:latest", sep = ""), intern = TRUE, ignore.stderr = TRUE), silent = TRUE))
  imagePresentMistnet <- suppressWarnings(try(system(paste("docker images -q adokter/vol2bird-mistnet:latest", sep = ""), intern = TRUE, ignore.stderr = TRUE), silent = TRUE))

  # return NA if docker command not found (docker not installed)
  if (class(imagePresent) == "try-error") {
    return(NA)
  }

  # return NaN if docker command executed, but threw an error status
  # this happens when Docker is installed, but when the Docker daemon is not running
  if ("status" %in% names(attributes(imagePresent))) {
    return(NaN)
  }

  # return NULL if the container is not available
  if (length(imagePresent) == 0) {
    return(NULL)
  }

  if (length(imagePresentMistnet) == 0) {
    image_name <- "vol2bird"
    .pkgenv$mistnet <- FALSE
  }
  else {
    image_name <- "vol2bird-mistnet"
    .pkgenv$mistnet <- TRUE
  }

  creationDate <- suppressWarnings(try(system(
    paste("docker inspect -f \"{{ .Created }}\" adokter/", image_name, sep = ""),
    intern = TRUE, ignore.stderr = TRUE
  ), silent = TRUE))

  # docker reports time stamps in Zulu (UTC) time
  creationDate <- as.POSIXct(creationDate, format = "%Y-%m-%dT%T", tz = "UTC")

  if (as.numeric(format(creationDate, "%Y")) < 2019) {
    # vol2bird version generated before 2019, i.e. version < 0.4.0
    vol2bird_version <- suppressWarnings(system(
      paste("docker run --rm adokter/", image_name, " bash -c \"vol2bird 2>&1 | grep Version\"", sep = ""),
      intern = TRUE
    ))
    vol2bird_version <- strsplit(trimws(vol2bird_version), split = " ")[[1]][2]
  }
  else {
    # vol2bird version >= 0.4.0, supporting --version argument
    vol2bird_version <- suppressWarnings(system(
      paste("docker run --rm adokter/", image_name, " bash -c \"vol2bird --version\"", sep = ""),
      intern = TRUE
    ))
    vol2bird_version <- strsplit(trimws(vol2bird_version), split = " ")[[1]][3]
  }
  return(numeric_version(vol2bird_version))
}
