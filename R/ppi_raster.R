#' ppiRaster: S4 subclass of terra::SpatRaster 
#'
#' Extends terra::SpatRaster by embedding radar metadata 
#' dedicated S4 slots, with formal validity checks and custom printing.
#' @name ppiRaster-class
#' @rdname ppiRaster-class
#' @exportClass ppiRaster
#' @import terra
#' @importMethodsFrom terra rast nlyr time<-
#' @importClassesFrom terra SpatRaster
setClass(
  "ppiRaster",
  contains = "SpatRaster",
  slots = c(
    radar    = "character",  # single radar identifier
    datetime = "POSIXct",     # one timestamp per layer
    geo      = "list"        # arbitrary geo properties list
  ),
  validity = function(object) {
    if (length(object@radar) != 1) {
      return("`radar` must be a length-1 character string")
    }
    if (length(object@datetime) != terra::nlyr(object)) {
      return("length(datetime) must equal the number of layers in the raster")
    }
    TRUE
  }
)

#' Construct a ppiRaster from a bioRad PPI
#'
#' @param ppi A list of class 'ppi' as returned by bioRad::project_as_ppi()
#' @return An object of class ppiRaster
#' @rdname ppiRaster-class
#' @export
ppi_to_ppiRaster <- function(ppi) {
  if (!inherits(ppi, "ppi")) {
    stop("Input must be a 'ppi' object from bioRad")
  }
  # convert sp::SpatialGridDataFrame to terra::SpatRaster
  r <- terra::rast(ppi$data)
  # assign time per layer
  ts <- rep(ppi$datetime, terra::nlyr(r))
  terra::time(r) <- ts
  # build the S4 object
  new(
    "ppiRaster",
    r,
    radar    = ppi$radar,
    datetime = terra::time(r),
    geo      = ppi$geo
  )
}

#' Convert an old ppi to a ppiRaster
#'
#' @param x A 'ppi' object
#' @return A ppiRaster object
#' @export
as.ppiRaster <- function(x, ...) UseMethod("as.ppiRaster")

#' @export
as.ppiRaster.ppi <- function(x, ...) ppi_to_ppiRaster(x)

#' @export
as.ppiRaster.ppiRaster <- function(x, ...) x

#' Show method for ppiRaster
#'
#' @param object An object of class ppiRaster
#' @rdname ppiRaster-class
#' @exportMethod show
setMethod(
  "show",
  "ppiRaster",
  function(object) {
    callNextMethod()  # prints SpatRaster summary
    cat("radar:   ", object@radar, "\n")
    cat("datetime:", paste(object@datetime, collapse=", "), "\n")
    if (!is.null(object@geo)) {
      cat("geo:     ", paste(names(object@geo), collapse=", "), "\n")
    }
  }
)

#' Extract radar metadata
#'
#' @param x A ppiRaster object
#' @return The radar slot
#' @export
setGeneric("radar", function(x) standardGeneric("radar"))
setMethod("radar", "ppiRaster", function(x) x@radar)

#' Extract acquisition times
#'
#' @param x A ppiRaster object
#' @return The datetime slot
#' @export
setGeneric("acquisitionTime", function(x) standardGeneric("acquisitionTime"))
setMethod("acquisitionTime", "ppiRaster", function(x) x@datetime)

#' Extract geographic properties
#'
#' @param x A ppiRaster object
#' @return The geo slot
#' @export
setGeneric("geoProps", function(x) standardGeneric("geoProps"))
setMethod("geoProps", "ppiRaster", function(x) x@geo)

#' @examples
#' \dontrun{
#' library(bioRad)
#' # project a scan
#' ppi_obj <- project_as_ppi(example_scan)
#' # convert to ppiRaster (S3 style)
#' pr1 <- as.ppiRaster(ppi_obj)
#' # or via constructor
#' pr2 <- ppi_to_ppiRaster(ppi_obj)
#' pr1
#' radar(pr1)
#' acquisitionTime(pr1)
#' geoProps(pr1)
#' }
