#' Project a scan (`scan`) or parameter (`param`) to a plan
#' position indicator (`ppi`)
#'
#' Make a plan position indicator (ppi)
#'
#' @param x An object of class `param` or `scan`.
#' @param grid_size Cartesian grid size in m.
#' @param range_max Maximum range in m.
#' @param ylim The range of latitudes to include.
#' @param xlim The range of longitudes to include.
#' @param project Whether to vertically project onto earth's surface.
#' @param raster (optional) RasterLayer with a CRS. When specified this raster topology is used for the output,
#' and `grid_size`, `range_max`, `xlim`, `ylim` are ignored.
#' @inheritParams beam_height
#'
#' @return An object of class '[ppi][summary.ppi]'.
#'
#' @export
#'
#' @details The returned PPI is in Azimuthal Equidistant Projection.
#'
#' @examples
#' # load a polar scan example object:
#' data(example_scan)
#' example_scan
#'
#' # plot the scan:
#' plot(example_scan)
#'
#' # make PPIs for all scan parameters in the scan:
#' ppi <- project_as_ppi(example_scan)
#'
#' # print summary info for the ppi:
#' ppi
#'
#' # plot the ppi:
#' plot(ppi)
#'
#' # extract the DBZH scan parameter of the volume to a new
#' # object 'param':
#' param <- get_param(example_scan, "VRADH")
#'
#' # make a ppi for the new 'param' object:
#' ppi <- project_as_ppi(param)
#'
#' # print summary info for this ppi:
#' ppi
#'
#' # plot the ppi:
#' plot(ppi)
project_as_ppi <- function(x, grid_size = 500, range_max = 50000,
                           project = TRUE, ylim = NULL, xlim = NULL, raster = NA, k = 4 / 3, re = 6378, rp = 6357) {
  UseMethod("project_as_ppi", x)
}


#' @describeIn project_as_ppi Project as `ppi` for a single scan parameter.
#'
#' @export
project_as_ppi.param <- function(x, grid_size = 500, range_max = 50000,
                                 project = TRUE, ylim = NULL, xlim = NULL, raster = NA, k = 4 / 3, re = 6378, rp = 6357) {
  # note: raster argument not used currently, as the raster is parsed through the
  # grid_size argument. May need to be refactored

  stopifnot(inherits(x, "param"))

  data <- sample_polar(x, grid_size, range_max, project, ylim, xlim, k = k, re = re, rp = rp)
  # copy the parameter's attributes
  geo <- attributes(x)$geo
  if (!inherits(grid_size, "RasterLayer")) {
    geo$bbox <- attributes(data)$bboxlatlon
  }
  geo$merged <- FALSE
  data <- list(
    radar = attributes(x)$radar, datetime = attributes(x)$datetime,
    data = data, geo = geo
  )
  class(data) <- "ppi"
  data
}

#' @describeIn project_as_ppi Project multiple `ppi`'s for all scan
#' parameters in a scan
#'
#' @export
project_as_ppi.scan <- function(x, grid_size = 500, range_max = 50000,
                                project = TRUE, ylim = NULL, xlim = NULL, raster = NA, k = 4 / 3, re = 6378, rp = 6357) {
  stopifnot(inherits(x, "scan"))

  if (!assertthat::are_equal(raster, NA)) {
    assertthat::assert_that(inherits(raster, "RasterLayer"))
  }

  if (inherits(raster, "RasterLayer")) {
    proj4string <- sp::CRS(paste("+proj=aeqd +lat_0=", x$geo$lat,
      " +lon_0=", x$geo$lon,
      " +units=m",
      sep = ""
    ))
    grid_size <- sp::spTransform(methods::as(methods::as(raster, "SpatialGrid"), "SpatialPoints"), proj4string)
  }
  data <- sample_polar(
    x$params[[1]], grid_size, range_max,
    project, ylim, xlim,
    k = k, re = re, rp = rp
  )
  # copy the parameter's geo list to attributes
  geo <- x$geo
  if ("bboxlatlon" %in% names(attributes(data))) {
    geo$bbox <- attributes(data)$bboxlatlon
  }
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
  if (inherits(data, "SpatialPoints")) {
    data <- sp::SpatialGridDataFrame(methods::as(raster, "SpatialGrid"), data@data)
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
  proj4string <- sp::CRS(paste("+proj=aeqd +lat_0=", attributes(param)$geo$lat,
    " +lon_0=", attributes(param)$geo$lon,
    " +units=m",
    sep = ""
  ))
  if (inherits(grid_size, c("RasterLayer", "SpatialPoints"))) {
    if (sp::proj4string(grid_size) != as.character(proj4string)) {
      gridTopo <- sp::spTransform(methods::as(methods::as(grid_size, "SpatialGrid"), "SpatialPoints"), proj4string)
    } else if (inherits(grid_size, "RasterLayer")) {
      gridTopo <- methods::as(methods::as(grid_size, "SpatialGrid"), "SpatialPoints")
    } else {
      gridTopo <- methods::as(grid_size, "SpatialPoints")
    }
  } else {
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
    gridTopo <- sp::GridTopology(cellcentre.offset, c(grid_size, grid_size), cells.dim)
  }
  # if projecting, account for elevation angle
  if (project) {
    elev <- attributes(param)$geo$elangle
  } else {
    elev <- 0
  }
  # get scan parameter indices, and extract data
  index <- polar_to_index(
    cartesian_to_polar(sp::coordinates(gridTopo), elev, k = k, lat = attributes(param)$geo$lat, re = re, rp = rp),
    rangebin = attributes(param)$geo$rscale,
    azimbin = attributes(param)$geo$ascale,
    azimstart = ifelse(is.null(attributes(param)$geo$astart), 0, attributes(param)$geo$astart),
    rangestart = ifelse(is.null(attributes(param)$geo$rstart), 0, attributes(param)$geo$rstart)
  )
  # set indices outside the scan's matrix to NA
  nrang <- dim(param)[1]
  nazim <- dim(param)[2]
  index$row[index$row > nrang] <- NA
  index$col[index$col > nazim] <- NA
  # rstart can result in locations outside of the radar scope close to the radar
  index$row[index$row < 1] <- NA
  stopifnot(all(index$col >= 1))
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

  if (inherits(grid_size, "RasterLayer")) {
    output <- sp::SpatialGridDataFrame(methods::as(grid_size, "SpatialGrid"), data)
  } else if (inherits(grid_size, "SpatialPoints")) {
    output <- sp::SpatialPointsDataFrame(grid_size, data)
  } else {
    output <- sp::SpatialGridDataFrame(
      grid = sp::SpatialGrid(
        grid = gridTopo,
        proj4string = proj4string
      ),
      data = data
    )
    attributes(output)$bboxlatlon <- bboxlatlon
  }
  output
}


#' A wrapper for [spTransform()].
#'
#' @param lon Longitude
#' @param lat Latitude
#' @param proj4string An object of class 'CRS', as defined in package `sp`.
#'
#' @keywords internal
#'
#' @return An object of class `SpatialPoints`.
wgs_to_proj <- function(lon, lat, proj4string) {
  xy <- data.frame(x = lon, y = lat)
  sp::coordinates(xy) <- c("x", "y")
  sp::proj4string(xy) <- sp::CRS("+proj=longlat +datum=WGS84")

  res <- sp::spTransform(xy, proj4string)
    
  # Check if the result is a SpatialPointsDataFrame
  if (inherits(res, "SpatialPointsDataFrame")) {
    # If it is, convert it to a SpatialPoints object
    rownames(res@bbox) <- c('x', 'y')
    colnames(res@coords) <- c('x', 'y')
    res <- sp::SpatialPoints(coords=res@coords, proj4string=res@proj4string, bbox=res@bbox)
  }
  return(res)
}
#' A wrapper for [spTransform()].
#'
#' @param x Longitude
#' @param y Latitude
#' @param proj4string An object of class 'CRS', as defined in package `sp`.
#'
#' @keywords internal
#'
#' @return An object of class `SpatialPoints`.
proj_to_wgs <- function(x, y, proj4string) {
  xy <- data.frame(lon = x, lat = y)
  sp::coordinates(xy) <- c("lon", "lat")
  sp::proj4string(xy) <- proj4string
  res <- NULL

  # Catch error when rgdal is not installed and sp_evolution_status is set to 0
  tryCatch({
    res <- sp::spTransform(xy, sp::CRS("+proj=longlat +datum=WGS84"))

    # Check if the result is a SpatialPointsDataFrame
    if (inherits(res, "SpatialPointsDataFrame")) {
      # If it is, convert it to a SpatialPoints object and correct names
      rownames(res@bbox) <- c('lon', 'lat')
      colnames(res@coords) <- c('lon', 'lat')
      res <- sp::SpatialPoints(coords=res@coords, proj4string=res@proj4string, bbox=res@bbox)
    }
    return(res)
  }, error = function(err) {
      if (grepl("package rgdal is required", err$message)) {
              err <- simpleError("spTransform failed. Try resetting sp_evolution_status: sp::set_evolution_status(2L)")
            } else {
              err <- simpleError("proj_to_wgs() failed")
            }
            stop(err)
      })
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

polar_to_index <- function(coords_polar, rangebin = 1, azimbin = 1, rangestart = 0, azimstart = 0) {
  row <- floor(1 + (coords_polar$range - rangestart) / c(rangebin))
  col <- floor(1 + ((coords_polar$azim - azimstart + 360) %% 360) / c(azimbin))
  data.frame(row = row, col = col)
}
