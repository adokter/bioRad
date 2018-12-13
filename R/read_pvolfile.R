#' Read a polar volume (\code{pvol}) from file
#'
#' @param file A string containing the path to a vertical profile generated
#' by \link[bioRad]{calculate_vp}.
#' @param sort A logical value, when \code{TRUE} sort scans ascending
#' by elevation.
#' @param param An atomic vector of character strings, containing the names
#' of scan parameters to read. To read all scan parameters use 'all'.
#' @param lat Latitude in decimal degrees of the radar position. If not
#' specified, value stored in file is used. If specified, value stored in
#' file is overwritten.
#' @param lon Longitude in decimal degrees of the radar position. If not
#' specified, value stored in file is used. If specified, value stored in
#' file is overwritten.
#' @param height Height of the center of the antenna in meters above sea
#' level. If not specified, value stored in file is used. If specified, value
#' stored in file is overwritten.
#' @param elev_min Minimum scan elevation to read in degrees.
#' @param elev_max Maximum scan elevation to read in degrees.
#' @param verbose A logical value, whether to print messages (\code{TRUE})
#' to console.
#' @param mount A character string with the mount point (a directory path)
#' for the Docker container.
#'
#' @return An object of class \link[=summary.pvol]{pvol}, which is a list
#' containing polar scans, i.e. objects of class \code{scan}
#'
#' @export
#'
#' @details
#' Scan parameters are named according to the OPERA data information
#' model (ODIM), see Table 16 in the
#' \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
#' Commonly available parameters are:
#' \describe{
#'  \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'  \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity [m/s]. Radial
#'    velocities towards the radar are negative, while radial velocities away
#'    from the radar are positive}
#'  \item{"\code{RHOHV}"}{Correlation coefficient [unitless]. Correlation
#'    between vertically polarized and horizontally polarized reflectivity
#'    factor}
#'  \item{"\code{PHIDP}"}{Differential phase [degrees]}
#'  \item{"\code{ZDR}"}{(Logged) differential reflectivity [dB]}
#' }
#'
#' @examples
#' # locate example volume file:
#' pvol <- system.file("extdata", "volume.h5", package = "bioRad")
#' # print the local path of the volume file:
#' pvol
#' # load the file:
#' vol <- read_pvolfile(pvol)
#' # print summary info for the loaded polar volume:
#' vol
#' # print summary info for the scans in the polar volume:
#' vol$scans
#' # copy the first scan to a new object 'scan'
#' scan <- vol$scans[[1]]
#' # print summary info for the new object:
#' scan
read_pvolfile <- function(file, param = c(
                            "DBZH", "VRADH", "VRAD", "RHOHV",
                            "ZDR", "PHIDP", "CELL"
                          ),
                          sort = TRUE, lat, lon, height, elev_min = 0,
                          elev_max = 90, verbose = TRUE,
                          mount = dirname(file)) {

  # input checks
  if (!is.logical(sort)) {
    stop("'sort' should be logical")
  }
  if (!missing(lat)) {
    if (!is.numeric(lat) || lat < -90 || lat > 90) {
      stop("'lat' should be numeric between -90 and 90 degrees")
    }
  }
  if (!missing(lon)) {
    if (!is.numeric(lon) || lat < -360 || lat > 360) {
      stop("'lon' should be numeric between -360 and 360 degrees")
    }
  }
  if (!missing(height)) {
    if (!is.numeric(height) || height < 0) {
      stop("'height' should be a positive number of meters above sea level")
    }
  }

  # check file type. If not ODIM hdf5, try to convert from RSL
  cleanup <- FALSE
  if (H5Fis_hdf5(file)) {
    if (!is.pvolfile(file)) {
      stop("Failed to read hdf5 file.")
    }
  } else {
    if (verbose) {
      cat("Converting using Docker...\n")
    }
    if (!.pkgenv$docker) {
      stop(
        "Requires a running Docker daemon.\nTo enable, start your ",
        "local Docker daemon, and run 'check_docker()' in R\n"
      )
    }
    file <- nexrad_to_odim_tempfile(file,
      verbose = verbose,
      mount = mount
    )
    if (!is.pvolfile(file)) {
      file.remove(file)
      stop("converted file contains errors")
    }
    cleanup <- TRUE
  }

  # extract scan groups
  scans <- h5ls(file, recursive = FALSE)$name
  scans <- scans[grep("dataset", scans)]

  # extract elevations, and make selection based on elevation
  elevs <- sapply(
    scans,
    function(x) {
      h5readAttributes(
        file,
        paste(x, "/where", sep = "")
      )$elangle
    }
  )
  scans <- scans[elevs >= elev_min & elevs <= elev_max]

  # extract attributes
  h5struct <- h5ls(file)
  h5struct <- h5struct[h5struct$group == "/", ]$name
  attribs.how <- attribs.what <- attribs.where <- NULL
  if ("how" %in% h5struct) {
    attribs.how <- h5readAttributes(file, "how")
  }
  if ("what" %in% h5struct) {
    attribs.what <- h5readAttributes(file, "what")
  }
  if ("where" %in% h5struct) {
    attribs.where <- h5readAttributes(file, "where")
  }

  vol.lat <- attribs.where$lat
  vol.lon <- attribs.where$lon
  vol.height <- attribs.where$height
  if (is.null(vol.lat)) {
    if (missing(lat)) {
      if (cleanup) {
        file.remove(file)
      }
      stop("latitude not found in file, provide 'lat' argument")
    } else {
      vol.lat <- lat
    }
  }

  if (is.null(vol.lon)) {
    if (missing(lon)) {
      if (cleanup) {
        file.remove(file)
      }
      stop("longitude not found in file, provide 'lon' argument")
    } else {
      vol.lon <- lon
    }
  }

  if (is.null(vol.height)) {
    if (missing(height)) {
      if (cleanup) {
        file.remove(file)
      }
      stop("antenna height not found in file, provide 'height' argument")
    } else {
      vol.height <- height
    }
  }
  geo <- list(lat = vol.lat, lon = vol.lon, height = vol.height)

  # convert some useful metadata
  datetime <- as.POSIXct(paste(attribs.what$date, attribs.what$time),
    format = "%Y%m%d %H%M%S", tz = "UTC"
  )
  sources <- strsplit(attribs.what$source, ",")[[1]]
  radar <- gsub("RAD:", "", sources[which(grepl("RAD:", sources))])

  # read scan groups
  data <- lapply(
    scans,
    function(x) {
      read_pvolfile_scan(file, x, param, radar, datetime, geo)
    }
  )
  # order by elevation
  if (sort) {
    data <- data[order(sapply(data, get_elevation_angles))]
  }

  # prepare output
  output <- list(
    radar = radar, datetime = datetime, scans = data,
    attributes = list(
      how = attribs.how, what = attribs.what,
      where = attribs.where
    ), geo = geo
  )
  class(output) <- "pvol"
  if (cleanup) {
    file.remove(file)
  }
  output
}

read_pvolfile_scan <- function(file, scan, param, radar, datetime, geo) {
  h5struct <- h5ls(file)
  h5struct <- h5struct[h5struct$group == paste("/", scan, sep = ""), ]$name
  groups <- h5struct[grep("data", h5struct)]

  # select which scan parameters to read
  if (length(param) == 1 && param == "all") {
    allParam <- TRUE
  } else {
    allParam <- FALSE
  }

  if (!allParam) {
    quantityNames <- sapply(
      groups,
      function(x) {
        h5readAttributes(
          file,
          paste(scan, "/", x, "/what",
            sep = ""
          )
        )$quantity
      }
    )
    groups <- groups[quantityNames %in% param]
    if (length(groups) == 0) {
      stop(paste("none of the requested scan parameters present in", file))
    }
  }

  # read attributes
  attribs.how <- attribs.what <- attribs.where <- NULL
  if ("how" %in% h5struct) {
    attribs.how <- h5readAttributes(file, paste(scan, "/how", sep = ""))
  }
  if ("what" %in% h5struct) {
    attribs.what <- h5readAttributes(file, paste(scan, "/what", sep = ""))
  }
  if ("where" %in% h5struct) {
    attribs.where <- h5readAttributes(file, paste(scan, "/where", sep = ""))
  }

  # add attributes to geo list
  geo$elangle <- attribs.where$elangle
  geo$rscale <- attribs.where$rscale
  geo$ascale <- 360 / attribs.where$nrays

  # read scan parameters
  quantities <- lapply(
    groups,
    function(x) {
      read_pvolfile_quantity(
        file,
        paste(scan, "/", x, sep = ""),
        radar, datetime, geo
      )
    }
  )
  quantityNames <- sapply(quantities, "[[", "quantityName")
  quantities <- lapply(quantities, "[[", "quantity")
  names(quantities) <- quantityNames

  output <- list(
    radar = radar, datetime = datetime, params = quantities,
    attributes = list(
      how = attribs.how, what = attribs.what,
      where = attribs.where
    ), geo = geo
  )
  class(output) <- "scan"
  output
}

read_pvolfile_quantity <- function(file, quantity, radar, datetime, geo) {
  data <- h5read(file, quantity)$data
  attr <- h5readAttributes(file, paste(quantity, "/what", sep = ""))
  data <- replace(data, data == as.numeric(attr$nodata), NA)
  data <- replace(data, data == as.numeric(attr$undetect), NaN)
  data <- as.numeric(attr$offset) + as.numeric(attr$gain) * data
  class(data) <- c("param", class(data))
  attributes(data)$radar <- radar
  attributes(data)$datetime <- datetime
  attributes(data)$geo <- geo
  attributes(data)$param <- attr$quantity
  list(quantityName = attr$quantity, quantity = data)
}
