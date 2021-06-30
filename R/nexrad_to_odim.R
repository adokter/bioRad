#' Convert a NEXRAD polar volume file to an ODIM HDF5 polar volume file
#'
#' Converts a NEXRAD polar volume input file to an ODIM HDF5 polar volume output
#' file (see [is.pvolfile()]). Requires a running
#' [Docker](https://www.docker.com/) daemon, see [check_docker()].
#'
#' @param pvolfile_nexrad Character. Path to a polar volume file in NEXRAD/RSL
#'   format.
#' @param pvolfile_odim Character. Path of the polar volume file in ODIM HDF5
#'   format to be written to disk.
#' @inheritParams calculate_vp
#'
#' @return `TRUE` on successful conversion.
#'
#' @export
#'
#' @seealso
#' * [check_docker()]
#' * [is.pvolfile()]
#'
#' @examples
#' \dontrun{
#' # Download a NEXRAD file and save as KBGM_example
#' download.file(
#'   "https://noaa-nexrad-level2.s3.amazonaws.com/2019/10/01/KBGM/KBGM20191001_000542_V06",
#'   "~/KBGM_example"
#' )
#'
#' # Convert to ODIM format
#' nexrad_to_odim("~/KBGM_example", "~/KBGM_example.h5")
#'
#' # Verify that we have generated a polar volume in ODIM HDF5 format
#' get_odim_object_type("~/KBGM_example.h5")
#'
#' # Clean up
#' file.remove("~/KBGM_example", "~/KBGM_example.h5")
#' }
nexrad_to_odim <- function(pvolfile_nexrad, pvolfile_odim, verbose = FALSE,
                           mount = dirname(pvolfile_nexrad), local_install) {
  if (!file.exists(dirname(pvolfile_odim))) {
    stop(paste("output directory", dirname(pvolfile_odim), "not found"))
  }
  if (file.access(dirname(pvolfile_odim), 2) == -1) {
    stop(paste("No write permission in directory", dirname(pvolfile_odim)))
  }
  pvol_tmp <- nexrad_to_odim_tempfile(pvolfile_nexrad, verbose, mount, local_install)
  file.rename(pvol_tmp, pvolfile_odim)
}

# Helper function
nexrad_to_odim_tempfile <- function(pvolfile, verbose = FALSE,
                                    mount = dirname(pvolfile), local_install) {
  # check input arguments
  if (file.access(mount, 0) == -1) {
    stop("Invalid 'mount' argument. Directory not found.")
  }
  if (file.access(mount, 2) == -1) {
    stop(paste(
      "Invalid 'mount' argument. No write permission in directory.",
      mount
    ))
  }
  if (!.pkgenv$docker && missing(local_install)) {
    stop(
      "Requires a running Docker daemon.\nTo enable, start your",
      "local Docker daemon, and run 'check_docker()' in R\n"
    )
  }
  if (!file.exists(pvolfile)) {
    stop("No such file or directory")
  }
  if (!length(verbose) == 1 || !is.logical(verbose)) {
    stop("Verbose argument should be one of TRUE or FALSE")
  }
  filedir <- dirname(normalizePath(pvolfile, winslash = "/"))
  if (!grepl(normalizePath(mount, winslash = "/"), filedir, fixed = TRUE)) {
    stop(
      "Mountpoint 'mount' has to be a parent directory of",
      "input file 'pvolfile'"
    )
  }
  pvol_tmp <- tempfile(tmpdir = filedir)
  if (file.access(filedir, mode = 2) < 0) {
    stop(paste("vol2bird requires write permission in", filedir))
  }
  if (missing(local_install)) {
    if (mount_docker_container(normalizePath(mount, winslash = "/")) != 0) {
      stop(paste("Failed to start vol2bird Docker container."))
    }
  }

  # prepare docker input filenames relative to mountpoint
  prefixstart <- if (mount == "/") 1 else 2
  prefix <- substring(
    filedir,
    prefixstart + nchar(normalizePath(mount, winslash = "/"))
  )
  if (nchar(prefix) > 0) {
    prefix <- paste(prefix, "/", sep = "")
  }
  pvolfile_docker <- paste(prefix, basename(pvolfile), sep = "")
  pvol_tmp_docker <- paste(prefix, basename(pvol_tmp), sep = "")

  # run vol2bird container
  if (.Platform$OS.type == "unix") {
    if (missing(local_install)) {
      result <- system(
        paste(
          "docker exec vol2bird bash -c 'cd data && rsl2odim ",
          pvolfile_docker, pvol_tmp_docker, "'"
        ),
        ignore.stdout = !verbose
      )
    } else{
      result <- system(paste("bash -l -c \"", paste(dirname(local_install),"/rsl2odim",sep=""), pvolfile, pvol_tmp, "\""), ignore.stdout = !verbose)
    }
  } else {
    result <- suppressWarnings(system(
      paste(
        "docker exec vol2bird bash -c \"cd data && rsl2odim ",
        pvolfile_docker, pvol_tmp_docker, "\""
      ),
      ignore.stdout = !verbose,
      show.output.on.console = TRUE
    ))
  }

  if (result != 0) {
    stop("Failed to complete conversion")
  }

  # return filename of generated temporary file
  return(pvol_tmp)
}
