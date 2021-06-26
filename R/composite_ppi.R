#' Create a composite of multiple plan position indicators (`ppi`)
#'
#' Combines multiple plan position indicators (`ppi`) into a single `ppi`. Can
#' be used to make a composite of `ppi`s from multiple radars.
#'
#' @inheritParams integrate_to_ppi
#' @param x A list of `ppi` objects.
#' @param param Character. Scan parameter to composite.
#' @param method Character. Compositing method, either `mean`, `min`, `max` or
#'   `idw`.
#' @param idw_max_distance Numeric. Maximum distance from the radar to consider
#'   in inverse distance weighting. Measurements beyond this distance will have
#'   a weighting factor of zero.
#' @param idp Numeric. Inverse distance weighting power.
#'
#' @return A `ppi` object.
#'
#' @export
#'
#' @details
#' The function can combine multiple `ppi`s of different scan elevations of the
#' same radar or `ppi`s of different radars. The coordinates of the returned
#' `ppi` object are in the WGS84 datum.
#'
#' The `method` parameter determines how values of different `ppi`s at the same
#' geographic location are combined:
#' * `mean`: Compute the average value.
#' * `max`: Compute the maximum value. If `ppi`s are of the same radar and the
#' same polar volume, this computes a max product, showing the maximum detected
#' signal at that geographic location.
#' * `min`: Compute the minimum value.
#' * `idw`: This option is useful primarily when compositing `ppi`s of multiple
#' radars. Performs an inverse distance weighting, where values are weighted
#' according to 1/(distance from the radar)^`idp`.
#'
#' @seealso
#' * [summary.ppi()]
#' * [project_as_ppi()]
#'
#' @examples
#' \dontrun{
#' # Locate and read the polar volume example file
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' pvol <- read_pvolfile(pvolfile)
#'
#' # Calculate a ppi for each elevation scan
#' ppis <- lapply(pvol$scans, project_as_ppi)
#'
#' # Overlay the ppis, calculating the maximum value observed
#' # across the available scans at each geographic location
#' composite <- composite_ppi(ppis, method = "max")
#'
#' # Download basemap
#' bm <- download_basemap(composite)
#'
#' # Plot the calculated max product on the basemap
#' map(composite, bm)
#' }
composite_ppi <- function(x, param = "DBZH", nx = 100, ny = 100, xlim, ylim, res, crs, raster = NA, method = "max", idp = 2, idw_max_distance=NA) {
  if (FALSE %in% sapply(x, is.ppi)) {
    stop("'composite' expects objects of class ppi only")
  }
  if (!is.count(nx) && missing(res)) stop("'nx' should be an integer")
  if (!is.count(ny) && missing(res)) stop("'ny' should be an integer")
  if (!missing(xlim)) {
    if (length(xlim) != 2 & !is.numeric(xlim)) stop("'xlim' should be a numeric vector of length two")
    if (is.na(xlim[1]) | is.na(xlim[2]) | xlim[1] > xlim[2]) stop("'xlim' should be a vector with two numeric values for lower and upper bound respectively")
  }
  if (!missing(ylim)) {
    if (length(ylim) != 2 & !is.numeric(ylim)) stop("'ylim' should be a numeric vector of length two")
    if (is.na(ylim[1]) | is.na(ylim[2]) | ylim[1] > ylim[2]) stop("'ylim' should be a vector with two numeric values for lower and upper bound respectively")
  }
  if (!missing(res)) {
    assert_that(is.numeric(res))
    assert_that(length(res) <= 2)
  } else {
    res <- NA
  }

  # check crs argument as in raster::raster()
  if (!missing(crs)) {
    crs <- CRS(as.character(raster::projection(crs)))
  }
  else {
    crs <- CRS("+proj=longlat +datum=WGS84")
  }

  ppis <- lapply(x, `[.ppi`, i = param)
  lons <- sapply(ppis, function(x) x$geo$bbox["lon", ])
  lats <- sapply(ppis, function(x) x$geo$bbox["lat", ])
  if(!missing(xlim)) lons <- xlim
  if(!missing(ylim)) lats <- ylim
  lons.radar <- sapply(ppis, function(x) x$geo$lon)
  lats.radar <- sapply(ppis, function(x) x$geo$lat)
  elangles <- sapply(ppis, function(x) x$geo$elangle)
  bbox <- matrix(c(min(lons), min(lats), max(lons), max(lats)),
    nrow = 2,
    ncol = 2, dimnames = dimnames(ppis[[1]]$geo$bbox)
  )

  if (!are_equal(raster, NA)) {
    r <- raster(raster)
  } else {
    if (missing(res) | is.na(res)) {
      r <- raster(ncols = nx, nrows = ny, ext = raster::extent(c(min(lons),max(lons),min(lats),max(lats))), crs = crs)
    }
    else {
      r <- raster(ncols = nx, nrows = ny, ext = raster::extent(c(min(lons),max(lons),min(lats),max(lats))), crs = crs, res = res)
    }
  }

  # initialize all values of the grid to NA
  raster::values(r) <- NA
  spGrid = as(r,'SpatialGridDataFrame')
  names(spGrid@data) <- names(ppis[[1]]$data)[1]

  # merge
  projs <- suppressWarnings(sapply(
    ppis,
    function(x) {
      over(
        spTransform(
          spGrid,
          CRS(proj4string(x$data))
        ),
        x$data
      )
    }
  ))

  if(method == "max") spGrid@data[, 1] <- do.call(function(...) pmax(..., na.rm = TRUE), projs)
  if(method == "min") spGrid@data[, 1] <- do.call(function(...) pmin(..., na.rm = TRUE), projs)
  if(method == "mean") as.data.frame(projs) %>% rowMeans(na.rm=T) -> spGrid@data[, 1]
  if(method == "idw"){
    brick_data = raster::brick(raster::brick(spGrid),nl=length(projs))
    brick_weights = brick_data
    #weights<-raster::pointDistance(as.matrix(data.frame(x=lons.radar,y=lats.radar)), coordinates(raster(spGrid)),lonlat=T)
    for(i in 1:length(projs)){
      brick_data <- raster::setValues(brick_data, projs[[i]], layer=i)
      latlon.radar <- unique(data.frame(lat=c(lats.radar), lon=c(lons.radar)))
      weights<-raster::pointDistance(as.matrix(data.frame(x=latlon.radar$lon,y=latlon.radar$lat))[i,], coordinates(raster(spGrid)),lonlat=T)
      if(!is.na(idw_max_distance)) weights[weights>idw_max_distance]=NA
      weights = 1/(weights^idp)

      brick_weights <- raster::setValues(brick_weights, weights, layer=i)
    }
    spGrid <- as(raster::weighted.mean(brick_data, brick_weights, na.rm=T),"SpatialGridDataFrame")
    names(spGrid@data) <- names(ppis[[1]]$data)[1]
  }

  ppi.out <- list(data = spGrid, geo = list(
    lat = lats.radar, lon = lons.radar,
    elangle = elangles, bbox = bbox,
    merged = TRUE
  ))
  class(ppi.out) <- "ppi"
  ppi.out
}
