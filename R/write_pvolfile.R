write_pvolfile <- function(pvol, file, overwrite = FALSE, same.type = TRUE) {
  assert_that(is.pvol(pvol))
  fid <- H5Fcreate(file)

  for (i in seq_along(pvol$scans)) {
    h5createGroup(fid, paste0("dataset", i))

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
      if (same.type) {
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
      h5writeAttribute(conv$offset, gid,"offset")
      h5writeAttribute(conv$nodata, gid,"nodata")
      h5writeAttribute(conv$undetect,gid,"undetect")
      H5Gclose(gid)
    }

    # write scan attributes
    attrgroupnames <- names(pvol$scans[[i]]$attributes)
    for(k in seq_along(attrgroupnames)){
      group <- paste0("dataset", i, "/", attrgroupnames[k])
      h5createGroup(fid, group)
      gid <- H5Gopen(fid, group)
      attrgroup <- pvol$scans[[i]]$attributes[[k]]
      attribnames <- names(attrgroup)
      for (l in seq_along(attribnames)){
        h5writeAttribute(attrgroup[[l]], gid, attribnames[l])
      }
      H5Gclose(gid)
    }
  }

  # write volume attributes
  attrgroupnames <- names(pvol$attributes)
  for(k in seq_along(attrgroupnames)){
    group <- paste0(attrgroupnames[k])
    h5createGroup(fid, group)
    gid <- H5Gopen(fid, group)
    attrgroup <- pvol$attributes[[k]]
    attribnames <- names(attrgroup)
    for (l in seq_along(attribnames)){
      h5writeAttribute(attrgroup[[l]], gid, attribnames[l])
      # # g <- H5Gopen(fid, paste(group, "/", attrgroup[[l]], sep = ""))
      # aid <- H5Acreate(gid, attribnames[l], h5space = H5Screate(type = "H5S_SCALAR"))
      # H5Awrite(aid)
    }
    H5Gclose(gid)
  }

  H5Fclose(fid)
}
