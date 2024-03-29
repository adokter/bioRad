#' Write a polar volume (`pvol`) object to ODIM HDF5 file
#'
#' @param pvol An object of class `pvol`.
#' @param file string. A filepath to write the `pvol` object to.
#' @param overwrite logical. Overwrites existing file when TRUE.
#' @param infer_dtype logical. By default (infer_dtype = FALSE) writes 'params'
#' back into ODIM HDF5 files with data stored in original data types. When TRUE
#' infers data type from the R object data type, at the cost of (heavily) inflated file sizes.
#'
#' @return 0 on success. A `pvol` object will be written to file in ODIM H5 format.
#'
#' @export
#'
#' @examples
#' # locate example volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # load the file:
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # write the file:
#' pvolfile_out <- paste0(tempdir(),"pvolfile_out.h5")
#' write_pvolfile(example_pvol, pvolfile_out)
#'
#' # clean up
#' file.remove(pvolfile_out)
write_pvolfile <- function(pvol, file, overwrite = FALSE, infer_dtype = FALSE) {
  assertthat::assert_that(is.pvol(pvol),
                          msg = "`pvol` must be an object of class `pvol`")
  if (!overwrite) {
    assertthat::assert_that(!file.exists(file),
      msg = "File already exists, use overwrite = TRUE to overwrite this file"
    )
  }
  fid <- rhdf5::H5Fcreate(file)

  for (i in seq_along(pvol$scans)) {
    rhdf5::h5createGroup(fid, paste0("dataset", i))

    # Write scan params
    for (j in seq_along(pvol$scans[[i]]$params)) {
      rhdf5::h5createGroup(fid, paste0("dataset", i, "/data", j))
      data <- pvol$scans[[i]]$params[[j]]
      conv <- attributes(data)$conversion

      d <- data
      class(d) <- "matrix"

      if (is.null(conv)) {
        conv <- list()
        longint <- FALSE
        # Check if data can be stored directly as integers
        # with a default nodata & undetect
        v <- as.vector(data)
        int <- all.equal(v, as.integer(v), na.rm = TRUE)
        range_bits <- max(v, na.rm = TRUE) - min(v, na.rm = TRUE)
        conv$gain <- 1
        conv$undetect <- 0
        if (isTRUE(int)) {
          conv$offset <- min(v, na.rm = TRUE) - 1
          if (range_bits <= 254) {
            conv$nodata <- 255
            conv$dtype <- "H5T_STD_U8BE"
          } else if (range_bits > 254 & range_bits <= 65534) {
            conv$nodata <- 65535
            conv$dtype <- "H5T_STD_I16BE"
          } else {
            longint <- TRUE # for 16-bit+ integers just use float
          }
        }

        if (!isTRUE(int) | longint) {
          # Store as 32-bit floats instead
          conv$offset <- floor(min(v, na.rm = TRUE)) - 1
          conv$nodata <- ceiling(max(v, na.rm = TRUE)) + 1 - conv$offset
          conv$dtype <- "H5T_IEEE_F32BE"
        }
      }

      d <- (d - conv$offset) / conv$gain
      d <- replace(d, is.nan(data), conv$undetect) # Replace NaNs
      d <- replace(d, is.na(data) & !is.nan(data), conv$nodata)
      dataname <- paste0("dataset", i, "/data", j, "/data")

      if (infer_dtype) conv$dtype <- NULL

      if (!is.null(conv$dtype)) {
        # this ensures converted NEXRAD files read as 64-bit floats to be
        # written as smaller 32-bit floats.
        if(grepl("H5T_IEEE_F64",conv$dtype)){
           conv$dtype=gsub("64","32",conv$dtype)
        }
        rhdf5::h5createDataset(fid, dataname, dim(pvol$scans[[i]]$params[[j]]),
          H5type = conv$dtype
        )
      } else {
        rhdf5::h5createDataset(fid, dataname, dim(pvol$scans[[i]]$params[[j]]))
      }

      rhdf5::h5write(d, fid, dataname)
      group <- paste0("dataset", i, "/data", j, "/what")
      rhdf5::h5createGroup(fid, group)
      gid <- rhdf5::H5Gopen(fid, group)
      rhdf5::h5writeAttribute(attributes(data)$param, gid, "quantity")
      rhdf5::h5writeAttribute(conv$gain, gid, "gain")
      rhdf5::h5writeAttribute(conv$offset, gid, "offset")
      rhdf5::h5writeAttribute(conv$nodata, gid, "nodata")
      rhdf5::h5writeAttribute(conv$undetect, gid, "undetect")
      rhdf5::H5Gclose(gid)
    }

    # Write scan attributes
    attrgroupnames <- names(pvol$scans[[i]]$attributes)
    attrgroupnames <- paste0("dataset", i, "/", attrgroupnames)
    write_group_attributes(fid, attrgroupnames, pvol$scans[[i]]$attributes)
  }

  # Write volume attributes
  attrgroupnames <- names(pvol$attributes)
  write_group_attributes(fid, attrgroupnames, pvol$attributes)
  rhdf5::H5Fclose(fid)
}

write_group_attributes <- function(fid, group, attrgroups) {
  for (k in seq_along(attrgroups)) {
    rhdf5::h5createGroup(fid, group[k])
    gid <- rhdf5::H5Gopen(fid, group[k])

    attrgroup <- attrgroups[k][[1]]
    attribnames <- names(attrgroup)

    for (l in seq_along(attribnames)) {
      rhdf5::h5writeAttribute(attrgroup[[l]][[1]], gid, attribnames[l])
    }
    rhdf5::H5Gclose(gid)
  }
}
