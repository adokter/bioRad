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
    attrgroupnames <- paste0("dataset", i, "/", attrgroupnames)
    write_group_attributes(fid, attrgroupnames, pvol$scans[[i]]$attributes)
    # for(k in seq_along(attrgroupnames)){
    #   group <- paste0("dataset", i, "/", attrgroupnames[k])
    #   h5createGroup(fid, group)
    #   gid <- H5Gopen(fid, group)
    #   attrgroup <- pvol$scans[[i]]$attributes[[k]]
    #   attribnames <- names(attrgroup)
    #   for (l in seq_along(attribnames)){
    #     h5writeAttribute(attrgroup[[l]], gid, attribnames[l])
    #   }
    #   H5Gclose(gid)
    # }
  }

  # write volume attributes
  attrgroupnames <- names(pvol$attributes)
  # groups <- paste0(attrgroupnames)
  write_group_attributes(fid, attrgroupnames, pvol$attributes)

  # for(k in seq_along(attrgroupnames)){
  #   group <- paste0(attrgroupnames[k])
  #   print(group)
  #   h5createGroup(fid, group)
  #   gid <- H5Gopen(fid, group)
  #   attrgroup <- pvol$attributes[[k]]
  #   attribnames <- names(attrgroup)
  #   for (l in seq_along(attribnames)){
  #     h5writeAttribute(attrgroup[[l]], gid, attribnames[l])
  #     # # g <- H5Gopen(fid, paste(group, "/", attrgroup[[l]], sep = ""))
  #     # aid <- H5Acreate(gid, attribnames[l], h5space = H5Screate(type = "H5S_SCALAR"))
  #     # H5Awrite(aid)
  #   }
  #   H5Gclose(gid)
  # }

  H5Fclose(fid)
}

write_group_attributes <- function(fid, group, attrgroups) {
  for (k in seq_along(attrgroups)) {
    print(group)
    h5createGroup(fid, group[k])
    gid <- H5Gopen(fid, group[k])

    attrgroup <- attrgroups[k][[1]]
    attribnames <- names(attrgroup)

    for (l in seq_along(attribnames)) {
      # print(dim(attrgroup[[l]]))
      # sid <- H5Screate_simple(c(1,1))
      # tid <- H5Tcopy("H5T_NATIVE_CHAR")
      # aid <- H5Acreate(gid, attribnames[l], tid, sid)
      # # aid <- H5Acreate(gid, attribnames[l], dtype_id = "H5T_NATIVE_CHAR",
      # #                  h5space = H5Screate(type = "H5S_SCALAR"))
      # H5Awrite(aid, attrgroup[[l]])
      # h5writeAttribute(attrgroup[[l]][[1]], gid, attribnames[l])
      h5wr(attrgroup[[l]][[1]], gid, attribnames[l])
    }
    H5Gclose(gid)
  }
}

h5wr <- function(attr, h5obj, name, size=NULL) {
  if (is.null(dim(attr))) {
    # dim(attr) = length(attr)
    d = length(attr)
  } else {
    d = dim(attr)
  }
  if (H5Aexists(h5obj, name)) {
    H5Adelete(h5obj, name)
  }
  size = NULL
  if (storage.mode(attr) == "character") {
    size = max(nchar(attr))+1
  }
  h5createAttribute(h5obj, name, dims = d, storage.mode = storage.mode(attr), size=size)
  h5attr <- H5Aopen(h5obj, name)

  DimMem <- dim(attr)
  DimMem <- d
  h5spaceMem <- H5Screate_simple(DimMem)

  try( { res <- H5Awrite(h5attr, attr) } )

  try( { H5Sclose(h5spaceMem) } )
  try( { H5Aclose(h5attr) } )
  invisible(res)
}

# h5wr <- function(attr, h5obj, name, size=NULL) {
#   if (is.null(dim(attr))) {
#     dim(attr) = length(attr)
#   }
#   if (H5Aexists(h5obj, name)) {
#     H5Adelete(h5obj, name)
#   }
#   size = NULL
#   if (storage.mode(attr) == "character") {
#     size = max(nchar(attr))+1
#   }
#   h5createAttribute(h5obj, name, dims = dim(attr), storage.mode = storage.mode(attr), size=size)
#   h5attr <- H5Aopen(h5obj, name)
#
#   DimMem <- dim(attr)
#   h5spaceMem <- H5Screate_simple(DimMem,NULL)
#
#   try( { res <- H5Awrite(h5attr, as.vector(attr)) } )
#
#   try( { H5Sclose(h5spaceMem) } )
#   try( { H5Aclose(h5attr) } )
#   invisible(res)
# }
