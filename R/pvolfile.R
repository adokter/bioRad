#' Check if a file is a polar volume (\code{pvol})
#'
#' Checker whether a file is a polar volume that can be read with package \pkg{bioRad}
#'
#' @param filename A string containing a filename
#' @export
#' @return TRUE when \code{filename} is a polar volume in readable format, otherwise FALSE
#' @examples
#' volume <- system.file("extdata", "volume.h5", package="bioRad")
#' is.pvolfile(volume)   #> TRUE
#'
is.pvolfile = function(filename){
  type=get_odim_object_type(filename)
  if(is.na(type)) return(FALSE)
  else return(type=="PVOL")
}

#' Check the ODIM data class of a polar volume file
#'
#' Checks which data class is contained in ODIM HDF5 file
#'
#' @param filename A string containing a filename
#' @export
#' @return character string "\code{pvol}" for polar volume, "\code{vp}" for vertical profile, otherwise \code{NA}
#' @examples
#' # locate a polar volume file
#' pvol <- system.file("extdata", "volume.h5", package="bioRad")
#' get_odim_object_type(pvol)   #> "pvol"
#'
get_odim_object_type = function(filename){
  if(!file.exists(filename)){
    warning(paste(filename,"does not exist"))
    return(NA)
  }
  if(!is.odimfile(filename)){
    warning(paste(filename,"is not a ODIM HDF5 file"))
    return(NA)
  }
  object=h5readAttributes(filename,"what")$object
  return(object)
}

is.odimfile = function(filename){
  if(!H5Fis_hdf5(filename)){
    warning(paste(filename,"is not a HDF5 file"))
    return(FALSE)
  }
  output = T
  groups=h5ls(filename,recursive=F)$name
  if(!("dataset1" %in% groups)){
    output = F
    warning(paste("HDF5 file",filename,"does not contain a /dataset1 group"))
  }
  if(!("what" %in% groups)){
    output = F
    warning(paste("HDF5 file",filename,"does not contain a /what group"))
  }
  else{
    object=h5readAttributes(filename,"what")$object
    if(is.null(object)){
      warning("'object' attribute not found in /what group")
      output=F
    }
  }
  if(!("how" %in% groups)){
    output = F
    warning(paste("HDF5 file",filename,"does not contain a /how group"))
  }
  if(!("where" %in% groups)){
    output = F
    warning(paste("HDF5 file",filename,"does not contain a /where group"))
  }
  return(output)
}
