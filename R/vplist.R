#' Concatenate vertical profiles (\code{vp}) into a list of vertical profiles (\code{vplist})
#'
#' @param ... objects of class \code{vp}
#' @export
#' @return an object of class \code{vplist}, see \link{readvp.list} for details
c.vp = function(...){
  vps=list(...)
  vptest=sapply(vps,function(x) is(x,"vp"))
  if(FALSE %in% vptest) {
    warning("non-vp objects found, returning a standard list...")
    return(vps)
  }
  # extract radar identifiers
  radars=unique(sapply(vps,'[[',"radar"))
  if(length(radars)>1) warning("Vertical profiles are not from a single radar")
  output=vps
  class(output)=c("vplist","list")
  output
}
