#' Class 'vplist': list of vertical profiles
#'
#' Class for list of vertical profiles
#' @param object object of class 'vplist'
#' @param x object of class 'vplist'
#' @param ... additional arguments affecting the summary produced.
#' @export
#' @method summary vplist
#' @details An object of class \code{vplist} is a list containing only \link[=summary.vp]{vp} objects.
#' By contrast, \link[=summary.vpts]{vpts} objects contain time-ordered profiles of a single radar
#' station. \code{vplist} objects can contain profiles of multiple radars.
summary.vplist=function(object, ...) print.vplist(object)

#' @rdname summary.vplist
#' @export
#' @return for \code{is.vplist}: \code{TRUE} if its argument is of class "\code{vplist}"
is.vplist <- function(x) inherits(x, "vplist")

#' Subset `vplist`
#'
#' Extract by index from a vplist
#'
#' @param x object of class 'vplist'
#' @param i indices specifying elements to extract
#' @export
`[.vplist` <- function(x,i) {
  stopifnot(inherits(x,"vplist"))
  output=unclass(x)[i]
  class(output)=c("vplist","list")
  return(output)
}

#' print method for class \code{vplist}
#'
#' @param x An object of class \code{vplist}, usually a result of a call to \link{readvp.list}
#' @keywords internal
#' @export
print.vplist=function(x,digits = max(3L, getOption("digits") - 3L), ...){
  stopifnot(inherits(x, "vplist"))
  # extract radar identifiers
  radar=unique(sapply(x,'[[',"radar"))
  # extract date-times
  dates=.POSIXct(do.call("c",lapply(x,'[[',"datetime")),tz="UTC")
  daterange=.POSIXct(c(min(dates),max(dates)),tz="UTC")
  cat("                   List of vertical profiles (class vplist)\n\n")
  cat("          radars: ",radar,"\n")
  cat("      # profiles: ",length(x),"\n")
  cat("time range (UTC): ",as.character(daterange[1]),"-",as.character(daterange[2]),"\n")
}

#' concatenate \code{vp} objects into a \code{vplist} object
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
