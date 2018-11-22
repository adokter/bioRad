#' Check if a local file is a polar volume (\code{pvol})
#'
#' Checker whether a file is a polar volume that can be read with
#' package \pkg{bioRad}
#'
#' @param file A string containing a file name.
#' @param filename Deprecated argument, use file instead.
#'
#' @return TRUE when \code{file} is a polar volume in readable format,
#' otherwise FALSE
#'
#' @export
#'
#' @examples
#' volume <- system.file("extdata", "volume.h5", package = "bioRad")
#' is.pvolfile(volume) # > TRUE
is.pvolfile <- function(file, filename = NULL) {

  # deprecate function arguments
  if (!missing(filename)) {
    warning("argument filename is deprecated; please use file instead.",
      call. = FALSE
    )
    file <- filename
  }

  type <- get_odim_object_type(file)
  if (is.na(type)) {
    return(FALSE)
  } else {
    return(type == "PVOL")
  }
}

#' Check the ODIM data class of a polar volume file
#'
#' Checks which data class is contained in ODIM HDF5 file
#'
#' @param file A string containing a file name.
#'
#' @return character string \code{pvol} for polar volume, \code{vp} for
#' vertical profile, otherwise \code{NA}
#'
#' @export
#'
#' @examples
#' # locate a polar volume file
#' pvol <- system.file("extdata", "volume.h5", package = "bioRad")
#' get_odim_object_type(pvol) # > "pvol"
get_odim_object_type <- function(file) {
  if (!file.exists(file)) {
    warning(paste(file, "does not exist"))
    return(NA)
  }
  if (!is.odimfile(file)) {
    warning(paste(file, "is not a ODIM HDF5 file"))
    return(NA)
  }
  object <- h5readAttributes(file, "what")$object
  return(object)
}

is.odimfile <- function(file) {
  if (!H5Fis_hdf5(file)) {
    warning(paste(file, "is not a HDF5 file"))
    return(FALSE)
  }
  output <- TRUE
  groups <- h5ls(file, recursive = FALSE)$name
  if (!("dataset1" %in% groups)) {
    output <- FALSE
    warning(paste(
      "HDF5 file", file,
      "does not contain a /dataset1 group"
    ))
  }

  if (!("what" %in% groups)) {
    output <- FALSE
    warning(paste(
      "HDF5 file", file,
      "does not contain a /what group"
    ))
  } else {
    object <- h5readAttributes(file, "what")$object

    if (is.null(object)) {
      warning("'object' attribute not found in /what group")
      output <- FALSE
    }
  }

  if (!("how" %in% groups)) {
    output <- FALSE
    warning(paste("HDF5 file", file, "does not contain a /how group"))
  }
  if (!("where" %in% groups)) {
    output <- FALSE
    warning(paste("HDF5 file", file, "does not contain a /where group"))
  }
  return(output)
}
