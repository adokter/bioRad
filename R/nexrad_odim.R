#' Convert a NEXRAD polar volume file to an ODIM polar volume file
#'
#' @param vol.in Polar volume input file in RSL format.
#' @param vol.out Filename for the polar volume in ODIM hdf5 format to be
#' generated
#'
#' @inheritParams calculate_vp
#'
#' @return \code{TRUE} on success
#'
#' @export
nexrad_to_odim <- function(vol.in, vol.out, verbose = FALSE,
                           mount = dirname(vol.in)) {
  if (!file.exists(dirname(vol.out))) {
    stop(paste("output directory", dirname(vol.out), "not found"))
  }
  if (file.access(dirname(vol.out),2) == -1) {
    stop(paste("No write permission in directory", dirname(vol.out)))
  }
  vol.tmp <- nexrad_to_odim_tempfile(vol.in, verbose, mount)
  file.rename(vol.tmp, vol.out)
}

nexrad_to_odim_tempfile <- function(vol.in, verbose = FALSE,
                                    mount = dirname(vol.in)) {
  # check input arguments
  if (file.access(mount, 0) == -1) {
    stop("Invalid 'mount' argument. Directory not found.")
  }
  if (file.access(mount, 2) == -1) {
    stop(paste("Invalid 'mount' argument. No write permission in directory.",
               mount))
  }
  if (!.pkgenv$docker) {
    stop("Requires a running Docker daemon.\nTo enable, start your",
         "local Docker daemon, and run 'check_docker()' in R\n")
  }
  if (!file.exists(vol.in)) {
    stop("No such file or directory")
  }
  if (!length(verbose) == 1 || !is.logical(verbose)) {
    stop("Verbose argument should be one of TRUE or FALSE")
  }
  filedir <- dirname(normalizePath(vol.in, winslash = "/"))
  if (!grepl(normalizePath(mount, winslash = "/"), filedir, fixed = TRUE)) {
    stop("Mountpoint 'mount' has to be a parent directory of",
         "input file 'vol.in'")
  }
  vol.tmp <- tempfile(tmpdir = filedir)
  if (file.access(filedir, mode = 2) < 0) {
    stop(paste("vol2bird requires write permission in", filedir))
  }
  if (mount_docker_container(normalizePath(mount, winslash = "/")) != 0) {
    stop(paste("Failed to start vol2bird Docker container."))
  }

  # prepare docker input filenames relative to mountpoint
  prefixstart <- if (mount == "/") 1 else 2
  prefix <- substring(filedir,
                      prefixstart + nchar(normalizePath(mount, winslash = "/")))
  if (nchar(prefix) > 0) {
    prefix <- paste(prefix, "/", sep = "")
  }
  vol.in.docker <- paste(prefix, basename(vol.in), sep = "")
  vol.tmp.docker <- paste(prefix, basename(vol.tmp), sep = "")

  # run vol2bird container
  if (.Platform$OS.type == "unix") {
    result <- system(
      paste("docker exec vol2bird bash -c 'cd data && rsl2odim ",
            vol.in.docker, vol.tmp.docker, "'"), ignore.stdout = !verbose)
  } else {
    result <- suppressWarnings(system(
      paste("docker exec vol2bird bash -c \"cd data && rsl2odim ",
            vol.in.docker, vol.tmp.docker, "\""), ignore.stdout = !verbose,
      show.output.on.console = TRUE))
  }

  if (result != 0) {
    stop("Failed to run nexrad_to_odim (rsl2odim) in Docker container.")
  }

  # return filename of generated temporary file
  return(vol.tmp)
}
