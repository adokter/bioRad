#' Create a composite of multiple plan position indicators (\code{ppi})
#'
#' Combines multiple plan position indicators (\code{ppi}) into a single
#' \code{ppi}. Can be used to make a composite of \code{ppi}'s from multiple
#' radars.
#'
#' @param x A list of \code{ppi}.
#' @param param Scan paramater to composite.
#' @param dim integer. Vector with number of cells in each spatial dimension.
#'
#' @return A \code{\link[=summary.ppi]{ppi}}.
#'
#' @export
#'
#' @details The latitude/longitude of the returned \code{ppi} use the WGS84
#' datum.
#'
#' @examples
#' # load the example polar scan:
#' data(example_scan)
#' # to be written ...
composite_ppi <- function(x, param = "DBZH", dim = c(100, 100)) {
  ppis <- lapply(x, `[.ppi`, i = param)
  if (FALSE %in% sapply(ppis, is.ppi)) {
    stop("'composite' expects objects of class ppi only")
  }
  lons <- sapply(ppis, function(x) x$geo$bbox["lon", ])
  lats <- sapply(ppis, function(x) x$geo$bbox["lat", ])
  lons.radar <- sapply(ppis, function(x) x$geo$lon)
  lats.radar <- sapply(ppis, function(x) x$geo$lat)
  elangles <- sapply(ppis, function(x) x$geo$elangle)
  bbox <- matrix(c(min(lons), min(lats), max(lons), max(lats)),
    nrow = 2,
    ncol = 2, dimnames = dimnames(ppis[[1]]$geo$bbox)
  )

  # define latlon grid
  wgs84 <- CRS("+proj=longlat +datum=WGS84")
  gridTopo <- GridTopology(
    bbox[, "min"],
    (bbox[, "max"] - bbox[, "min"]) / dim,
    dim
  )
  grid <- SpatialGrid(grid = gridTopo, proj4string = wgs84)

  # initialize all values of the grid to NA
  spGrid <- SpatialGridDataFrame(
    grid = grid,
    data = data.frame(z = rep(1, dim[1] * dim[2]))
  )
  names(spGrid@data)[1] <- names(ppis[[1]]$data)[1]

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

  spGrid@data[, 1] <- do.call(function(...) pmax(..., na.rm = TRUE), projs)

  ppi.out <- list(data = spGrid, geo = list(
    lat = lats.radar, lon = lons.radar,
    elangle = elangles, bbox = bbox,
    merged = TRUE
  ))
  class(ppi.out) <- "ppi"
  ppi.out
}
