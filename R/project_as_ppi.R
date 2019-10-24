#' Project a scan (\code{scan}) or parameter (\code{param}) to a plan
#' position indicator (\code{ppi})
#'
#' Make a plan position indicator (ppi)
#'
#' @param x An object of class \code{param} or \code{scan}.
#' @param grid_size Cartesian grid size in m.
#' @param range_max Maximum range in m.
#' @param ylim The range of latitudes to include.
#' @param xlim The range of longitudes to include.
#' @param project Whether to vertically project onto earth's surface.
#' @inheritParams beam_height
#'
#' @return An object of class '\link[=summary.ppi]{ppi}'.
#'
#' @export
#'
#' @details The returned PPI is in Azimuthal Equidistant Projection.
#'
#' @examples
#' # load a polar scan example object
#' data(example_scan)
#' example_scan
#' # make PPIs for all scan parameters in the scan:
#' ppi <- project_as_ppi(example_scan)
#' # print summary info for the ppi:
#' ppi
#' # copy the first scan parameter of the first scan in the volume to a new
#' # object 'param':
#' param <- example_scan$params[[1]]
#' # make a ppi for the new 'param' object:
#' ppi <- project_as_ppi(param)
#' # print summary info for this ppi:
#' ppi
project_as_ppi <- function(x, grid_size = 500, range_max = 50000,
                           project = FALSE, ylim = NULL, xlim = NULL, k = 4 / 3, re = 6378, rp = 6357) {
  UseMethod("project_as_ppi", x)
}


#' @describeIn project_as_ppi Project as \code{ppi} for a single scan parameter.
#'
#' @export
project_as_ppi.param <- function(x, grid_size = 500, range_max = 50000,
                                 project = FALSE, ylim = NULL, xlim = NULL, k = 4 / 3, re = 6378, rp = 6357) {
  stopifnot(inherits(x, "param"))

  data <- sample_polar(x, grid_size, range_max, project, ylim, xlim, k = k, re = re, rp = rp)
  # copy the parameter's attributes
  geo <- attributes(x)$geo
  geo$bbox <- attributes(data)$bboxlatlon
  geo$merged <- FALSE
  data <- list(
    radar = attributes(x)$radar, datetime = attributes(x)$datetime,
    data = data, geo = geo
  )
  class(data) <- "ppi"
  data
}

#' @describeIn project_as_ppi Project multiple \code{ppi}'s for all scan
#' parameters in a scan
#'
#' @export
project_as_ppi.scan <- function(x, grid_size = 500, range_max = 50000,
                                project = FALSE, ylim = NULL, xlim = NULL, k = 4 / 3, re = 6378, rp = 6357) {
  stopifnot(inherits(x, "scan"))

  data <- sample_polar(
    x$params[[1]], grid_size, range_max,
    project, ylim, xlim,
    k = k, re = re, rp = rp
  )
  # copy the parameter's geo list to attributes
  geo <- x$geo
  geo$bbox <- attributes(data)$bboxlatlon
  geo$merged <- FALSE
  if (length(x$params) > 1) {
    alldata <- lapply(
      x$params,
      function(param) {
        sample_polar(
          param, grid_size, range_max,
          project, ylim, xlim,
          k = k, re = re, rp = rp
        )
      }
    )
    data <- do.call(cbind, alldata)
  }
  data <- list(
    radar = x$radar, datetime = x$datetime,
    data = data, geo = geo
  )
  class(data) <- "ppi"
  data
}


sample_polar <- function(param, grid_size, range_max, project, ylim, xlim, k = 4 / 3, re = 6378, rp = 6357) {
  # proj4string=CRS(paste("+proj=aeqd +lat_0=",attributes(param)$geo$lat," +lon_0=",attributes(param)$geo$lon," +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep=""))
  proj4string <- CRS(paste("+proj=aeqd +lat_0=", attributes(param)$geo$lat,
    " +lon_0=", attributes(param)$geo$lon,
    " +units=m",
    sep = ""
  ))
  bboxlatlon <- proj_to_wgs(
    c(-range_max, range_max),
    c(-range_max, range_max),
    proj4string
  )@bbox
  if (!missing(ylim) & !is.null(ylim)) {
    bboxlatlon["lat", ] <- ylim
  }
  if (!missing(xlim) & !is.null(xlim)) {
    bboxlatlon["lon", ] <- xlim
  }
  if (missing(ylim) & missing(xlim)) {
    cellcentre.offset <- -c(range_max, range_max)
    cells.dim <- ceiling(rep(2 * range_max / grid_size, 2))
  } else {
    bbox <- wgs_to_proj(bboxlatlon["lon", ], bboxlatlon["lat", ], proj4string)
    cellcentre.offset <- c(
      min(bbox@coords[, "x"]),
      min(bbox@coords[, "y"])
    )
    cells.dim <- c(
      ceiling((max(bbox@coords[, "x"]) -
        min(bbox@coords[, "x"])) / grid_size),
      ceiling((max(bbox@coords[, "y"]) -
        min(bbox@coords[, "y"])) / grid_size)
    )
  }
  # define cartesian grid
  gridTopo <- GridTopology(cellcentre.offset, c(grid_size, grid_size), cells.dim)
  # if projecting, account for elevation angle
  if (project) {
    elev <- attributes(param)$geo$elangle
  } else {
    elev <- 0
  }
  # get scan parameter indices, and extract data
  index <- polar_to_index(
    cartesian_to_polar(coordinates(gridTopo), elev, k = k, lat = attributes(param)$geo$lat, re = re, rp = rp),
    attributes(param)$geo$rscale,
    attributes(param)$geo$ascale
  )
  # set indices outside the scan's matrix to NA
  nrang <- dim(param)[1]
  nazim <- dim(param)[2]
  index$row[index$row > nrang] <- NA
  index$col[index$col > nazim] <- NA
  # convert 2D index to 1D index
  index <- (index$col - 1) * nrang + index$row
  data <- as.data.frame(param[index])

  #  data <- data.frame(mapply(
  #    function(x, y) {
  #      safe_subset(param, x, y)
  #    },
  #    x = index$row,
  #    y = index$col
  #  ))

  colnames(data) <- attributes(param)$param
  output <- SpatialGridDataFrame(
    grid = SpatialGrid(
      grid = gridTopo,
      proj4string = proj4string
    ),
    data = data
  )
  attributes(output)$bboxlatlon <- bboxlatlon
  output
}


#' A wrapper for \code{\link{spTransform}}.
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param proj4string An object of class 'CRS', as defined in package \code{sp}.
#'
#' @keywords internal
#'
#' @return An object of class \code{SpatialPoints}.
wgs_to_proj <- function(lon, lat, proj4string) {
  xy <- data.frame(x = lon, y = lat)
  coordinates(xy) <- c("x", "y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, proj4string)
  return(res)
}

#' A wrapper for \code{\link{spTransform}}.
#'
#' @param x Longitude
#' @param y Latitude
#' @param proj4string An object of class 'CRS', as defined in package \code{sp}.
#' @keywords internal
#' @return An object of class \code{SpatialPoints}.
proj_to_wgs <- function(x, y, proj4string) {
  xy <- data.frame(lon = x, lat = y)
  coordinates(xy) <- c("lon", "lat")
  proj4string(xy) <- proj4string
  res <- spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
  return(res)
}

cartesian_to_polar <- function(coords, elev = 0, k = 4 / 3, lat = 35, re = 6378, rp = 6357) {
  range <- beam_range(sqrt(coords[, 1]^2 + coords[, 2]^2), elev, k = k, lat = lat, re = re, rp = rp)
  azim <- (0.5 * pi - atan2(coords[, 2], coords[, 1])) %% (2 * pi)
  data.frame(range = range, azim = azim * 180 / pi)
}

safe_subset <- function(data, indexx, indexy) {
  datadim <- dim(data)
  if (indexx < 1 || indexx > datadim[1] || indexy < 1 || indexy > datadim[2]) {
    out <- NA
  } else {
    out <- data[indexx, indexy]
  }
  out
}

polar_to_index <- function(coords_polar, rangebin = 1, azimbin = 1) {
  row <- floor(1 + coords_polar$range / c(rangebin))
  col <- floor(1 + coords_polar$azim / c(azimbin))
  data.frame(row = row, col = col)
}
