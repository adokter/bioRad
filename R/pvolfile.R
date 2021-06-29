#' Check if a file is a polar volume (`pvol`)
#'
#' Checks whether a file is a polar volume (`pvol`) in the ODIM HDF5 format that
#' can be read with bioRad. Evaluates to `FALSE` for NEXRAD and IRIS RAW polar
#' volume file (see [nexrad_to_odim()]).
#'
#' @param file Character. Path of the file to check.
#'
#' @return `TRUE` for a polar volume file in readable format, otherwise `FALSE`.
#'
#' @export
#'
#' @seealso
#' * [read_pvolfile()]
#' * [get_odim_object_type()]
#' * [is.pvol()]
#'
#' @examples
#' # Locate the polar volume example file
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # Check if it is a pvolfile
#' is.pvolfile(pvolfile)
is.pvolfile <- function(file) {
  type <- get_odim_object_type(file)
  if (is.na(type)) {
    warning(paste("is.pvolfile() must be a hdf5 file. Polar volume files in ",
                  "other data formats not yet recognized)."))
    return(FALSE)
  } else {
    return(type == "PVOL")
  }
}

#' Check the `data` object contained in a ODIM HDF5 file
#'
#' Checks which data class is contained in a ODIM HDF5 file. See [ODIM
#' specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf),
#' Table 2 for a full list of existing ODIM file object types.
#'
#' @param file Character. Path of the file to check.
#'
#' @return Character. `PVOL` for polar volume, `VP` for vertical profile,
#'   otherwise `NA`.
#'
#' @export
#'
#' @seealso
#' * [is.pvolfile()]
#' * [is.vpfile()]
#'
#' @examples
#' # Locate the polar volume example file
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # Check the data type
#' get_odim_object_type(pvolfile)
get_odim_object_type <- function(file) {
  if (!file.exists(file)) {
    warning(glue("Can't find {file}"))
    return(NA)
  }
  if (!is.odimfile(file)) {
    # Errors are handled by is.odimfile()
    return(NA)
  }
  object <- h5readAttributes(file, "what")$object
  # current implementation of write_pvolfile stores string attributes as
  # single element arrays. This line guarantees that for files written with write_pvolfile
  # the output class is character instead of an array with a single character element.
  object <- object[1]
  return(object)
}

is.odimfile <- function(file) {
  if (!H5Fis_hdf5(file)) {
    warning(glue("{file} is not an HDF5 file."))
    return(FALSE)
  }
  output <- TRUE
  groups <- h5ls(file, recursive = FALSE)$name
  if (!("dataset1" %in% groups)) {
    output <- FALSE
    warning(
      glue("HDF5 file {file} does not contain a `/dataset1` group.")
    )
  }

  if (!("what" %in% groups)) {
    output <- FALSE
    warning(
      glue("HDF5 file {file} does not contain a `/what` group.")
    )
  } else {
    object <- h5readAttributes(file, "what")$object
    if (is.null(object)) {
      warning(
        glue("HDF5 file {file} does not contain an `object` attribute in the ",
        "`/what` group.")
      )
      output <- FALSE
    }
  }

  if (!("how" %in% groups)) {
    # accepting a missing /how group
    warning(
      glue("HDF5 file {file} does not contain a `/how` group.")
    )
  }
  if (!("where" %in% groups)) {
    output <- FALSE
    warning(
      glue("HDF5 file {file} does not contain a `/where` group.")
    )
  }
  return(output)
}
