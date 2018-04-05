#' fetch a profile quantity
#' @param x a vp,vplist or vpts object
#' @param quantity a profile quantity, one of
#' \code{"HGHT"},\code{"u"},\code{"v"},\code{"w"},\code{"ff"},
#' \code{"dd"},\code{"sd_vvp"},\code{"gap"},\code{"dbz"},\code{"eta"},
#' \code{"dens"},\code{"DBZH"},\code{"n"},\code{"n_all"},\code{"n_dbz"},\code{"n_dbz_all"}.
#' @details This function grabs any of the data quantities stored in \link[=summary.vp]{vp},
#' \link[=summary.vplist]{vplist} or \link[=summary.vpts]{vpts} objects.
#'
#' See the documentation of the vertical profile \link[=summary.vp]{vp} class
#' for a description of each of these quantities.
#' @export
fetch=function(x, quantity) UseMethod("fetch", x)

#' @rdname fetch
#' @export
#' @return class \code{vp}: a named vector for the requested quantity
fetch.vp=function(x, quantity="dens"){
  stopifnot(inherits(x,"vp"))
  output=x$data[quantity][,1]
  names(output)=x$data$HGHT
  if(quantity == "eta"){
    output[x$data$sd_vvp<sd_vvp(x)]=0
    return(output)
  }
  if(quantity == "dbz"){
    output[x$data$sd_vvp<sd_vvp(x)]=-Inf
    return(output)
  }
  if(quantity %in% c("ff","u","v","w","dd")){
    output[x$data$sd_vvp<sd_vvp(x)]=NaN
    return(output)
  }
  return(output)
}

#' @rdname fetch
#' @export
#' @return class \code{vplist}: a list of a named vectors for the requested quantity
fetch.vplist <- function(x,quantity="dens") {
  stopifnot(inherits(x,"vplist"))
  lapply(x,fetch.vp,quantity=quantity)
}

#' @rdname fetch
#' @export
#' @return class \code{vpts}: a (height x time) matrix of the requested quantity
fetch.vpts=function(x, quantity="dens"){
  ## this function should checkout both the gap and sd_vvp flags
  stopifnot(inherits(x,"vpts"))
  output=x$data[quantity][[1]]
  rownames(output)=x$heights
  colnames(output)=as.character(x$dates)
  if(quantity == "eta"){
    output[x$data$sd_vvp<sd_vvp(x)]=0
    return(output)
  }
  if(quantity == "dbz"){
    output[x$data$sd_vvp<sd_vvp(x)]=-Inf
    return(output)
  }
  if(quantity %in% c("ff","u","v","w","dd")){
    output[x$data$sd_vvp<sd_vvp(x)]=NaN
    return(output)
  }
  return(output)
}
