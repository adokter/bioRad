write_pvolfile <- function(pvol, file, overwrite = FALSE, same.type = TRUE) {
  assert_that(is.pvol(pvol))
  fid <- H5Fcreate(file)

  for (i in seq_along(pvol$scans)) {
    h5createGroup(fid, paste0("dataset", i))

    # Write scan params
    for (j in seq_along(pvol$scans[[i]]$params)) {
      h5createGroup(fid, paste0("dataset", i, "/data", j))
      data <- pvol$scans[[i]]$params[[j]]
      conv <- attributes(data)$conversion
      d <- data
      class(d) <- "matrix"
      d <- (d - conv$offset) / conv$gain
      d <- replace(d, is.nan(data), conv$undetect)  # Replace NaNs
      d <- replace(d, is.na(data) & !is.nan(data), conv$nodata)
      dataname <- paste0("dataset", i, "/data", j, "/data")

      if (!same.type) {
        # If using non-standard params (eg after using calculate_param())
        # set dtype to NULL to infer dtypes.
        conv$dtype <- NULL
      }

      if (!is.null(conv$dtype)) {
        h5createDataset(fid, dataname, dim(pvol$scans[[i]]$params[[j]]),
                        H5type = conv$dtype)
      } else {
        h5createDataset(fid, dataname, dim(pvol$scans[[i]]$params[[j]]))
      }

      h5write(d, fid, dataname)
      group <- paste0("dataset", i, "/data", j, "/what")
      h5createGroup(fid, group)
      gid <- H5Gopen(fid, group)
      h5writeAttribute(attributes(data)$param, gid, "quantity")
      h5writeAttribute(conv$gain, gid, "gain")
      h5writeAttribute(conv$offset, gid, "offset")
      h5writeAttribute(conv$nodata, gid, "nodata")
      h5writeAttribute(conv$undetect,gid, "undetect")
      H5Gclose(gid)
    }

    # Write scan attributes
    attrgroupnames <- names(pvol$scans[[i]]$attributes)
    attrgroupnames <- paste0("dataset", i, "/", attrgroupnames)
    write_group_attributes(fid, attrgroupnames, pvol$scans[[i]]$attributes)
  }

  # Write volume attributes
  attrgroupnames <- names(pvol$attributes)
  write_group_attributes(fid, attrgroupnames, pvol$attributes)
  H5Fclose(fid)
}

write_group_attributes <- function(fid, group, attrgroups) {
  for (k in seq_along(attrgroups)) {
    h5createGroup(fid, group[k])
    gid <- H5Gopen(fid, group[k])

    attrgroup <- attrgroups[k][[1]]
    attribnames <- names(attrgroup)

    for (l in seq_along(attribnames)) {
      h5writeAttribute(attrgroup[[l]][[1]], gid, attribnames[l])
    }
    H5Gclose(gid)
  }
}
