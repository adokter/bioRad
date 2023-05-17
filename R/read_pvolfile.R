#' Read a polar volume (`pvol`) from file
#'
#' @param file A string containing the path to a polar volume file
#' @param sort A logical value, when `TRUE` sort scans ascending
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
#' @param verbose A logical value, whether to print messages (`TRUE`)
#' to console.
#' @param mount (deprecated) A character string with the mount point (a directory path)
#' for the Docker container.
#' @param local_install (deprecated) String with path to local vol2bird installation,
#' to use local installation instead of Docker container
#'
#' @return An object of class [pvol][summary.pvol], which is a list
#' containing polar scans, i.e. objects of class `scan`
#'
#' @export
#'
#' @details
#' Scan parameters are named according to the OPERA data information
#' model (ODIM), see Table 16 in the
#' [ODIM specification](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf).
#' Commonly available parameters are:
#' * `DBZH`, `DBZ`: (Logged) reflectivity factor (dBZ)
#' * `TH`, `T`: (Logged) uncorrected reflectivity factor (dBZ)
#' * `VRADH`, `VRAD`: Radial velocity (m/s). Radial
#'    velocities towards the radar are negative, while radial velocities away
#'    from the radar are positive
#' * `RHOHV`: Correlation coefficient (unitless). Correlation
#'    between vertically polarized and horizontally polarized reflectivity
#'    factor
#' * `PHIDP`: Differential phase (degrees)
#' * `ZDR`: (Logged) differential reflectivity (dB)
#'
#' @examples
#' # locate example volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # print the local path of the volume file:
#' pvolfile
#'
#' # load the file:
#' example_pvol <- read_pvolfile(pvolfile)
#'
#' # print summary info for the loaded polar volume:
#' example_pvol
#'
#' # print summary info for the scans in the polar volume:
#' example_pvol$scans
#'
#' # copy the first scan to a new object 'scan'
#' scan <- example_pvol$scans[[1]]
#'
#' # print summary info for the new object:
#' scan
read_pvolfile <- function(file, param = c(
                            "DBZH", "DBZ", "VRADH", "VRAD", "WRADH", "WRAD", "TH", "T", "RHOHV",
                            "ZDR", "PHIDP", "CELL", "BIOLOGY", "WEATHER", "BACKGROUND"
                          ),
                          sort = TRUE, lat, lon, height, elev_min = 0,
                          elev_max = 90, verbose = TRUE,
                          mount = dirname(file), local_install) {
  if(!missing(local_install)) warning("argument 'local_install' has been deprecated")
  if(!file.exists(file)){
     stop(paste0("'",file,"' does not exist in current working directory ('",getwd(),"')."))
  }
  tryCatch(read_pvolfile_body(
    file, param, sort, lat, lon,
    height, elev_min, elev_max,
    verbose, mount, local_install
  ),
  error = function(err) {
    rhdf5::h5closeAll()
    stop(err)
  }
  )
}

# this is the actual function read_pvolfile, without error handling that checks
# for open HDF5 files
read_pvolfile_body <- function(file, param = c(
                                 "DBZH", "DBZ", "VRADH", "VRAD", "TH", "T", "RHOHV",
                                 "ZDR", "PHIDP", "CELL", "BIOLOGY", "WEATHER", "BACKGROUND"
                               ),
                               sort = TRUE, lat, lon, height, elev_min = 0,
                               elev_max = 90, verbose = TRUE,
                               mount = dirname(file), local_install) {
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

  # check file type. If not ODIM HDF5, try to convert from RSL
  cleanup <- FALSE
  if (H5Fis_hdf5(file)) {
    if (!is.pvolfile(file)) {
      stop("Failed to read HDF5 file.")
    }
  } else {
    pvol_tmp <- tempfile()

    config <- vol2birdR::vol2bird_config()
    vol2birdR::rsl2odim(file=file, config=config, pvolfile_out=pvol_tmp, verbose=verbose)

    if (!is.pvolfile(pvol_tmp)) {
      file.remove(pvol_tmp)
      stop("converted file contains errors")
    }
    cleanup <- TRUE
    file <- pvol_tmp
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

  # construct wavelength attribute from frequency attribute if possible:
  if(is.null(attribs.how$wavelength) & !is.null(attribs.how$frequency)){
    speed_of_light = 299792458
    attribs.how$wavelength = 100*speed_of_light/attribs.how$frequency
  }

  vol.lat <- c(attribs.where$lat) # need the c() to convert single element matrix to single element vector
  vol.lon <- c(attribs.where$lon)
  vol.height <- c(attribs.where$height)
  if (is.null(vol.lat)) {
    if (missing(lat)) {
      if (cleanup) {
        file.remove(file)
      }
      stop("latitude not found in file, provide 'lat' argument")
    }
  }
  if (!missing(lat)) {
    vol.lat <- lat
  }

  if (is.null(vol.lon)) {
    if (missing(lon)) {
      if (cleanup) {
        file.remove(file)
      }
      stop("longitude not found in file, provide 'lon' argument")
    }
  }
  if (!missing(lon)) {
    vol.lon <- lon
  }

  if (is.null(vol.height)) {
    if (missing(height)) {
      if (cleanup) {
        file.remove(file)
      }
      stop("antenna height not found in file, provide 'height' argument")
    }
  }
  if (!missing(height)) {
    vol.height <- height
  }

  geo <- list(lat = vol.lat, lon = vol.lon, height = vol.height)

  # convert some useful metadata
  datetime <- as.POSIXct(paste(attribs.what$date, attribs.what$time),
    format = "%Y%m%d %H%M%S", tz = "UTC"
  )
  if(is.null(attribs.what$source)) attribs.what$source=""
  sources <- strsplit(attribs.what$source, ",")[[1]]
  radar <- gsub("NOD:", "", sources[which(grepl("NOD:", sources))])
  if (length(radar) == 0) {
    radar <- gsub("RAD:", "", sources[which(grepl("RAD:", sources))])
    if (length(radar) == 0) {
      radar <- gsub("WMO:", "", sources[which(grepl("WMO:", sources))])
      if (length(radar) == 0) {
        radar <- "unknown"
      }
    }
  }


  # write height, lat, lon attributes (update with potential user-defined values)
  attribs.where$height <- vol.height
  attribs.where$lat <- vol.lat
  attribs.where$lon <- vol.lon

  # assemble attributes in list
  attributes <- list(
    how = attribs.how, what = attribs.what,
    where = attribs.where
  )

  # read scan groups
  data <- lapply(
    scans,
    function(x) {
      read_pvolfile_scan(file, x, param, radar, datetime, geo, attributes)
    }
  )

  # filter out NULL output from read_pvolfile_scan
  valid_scans <- which(!sapply(data, is.null))
  assert_that(length(valid_scans) > 0, msg = paste("none of the requested scan parameters found in file", file))
  if(length(valid_scans) < length(scans)){
    warning(paste("ignoring",length(scans)-length(valid_scans),"scan(s) in file",file,"because requested scan parameter(s) are missing."))
  }
  data <- data[valid_scans]

  # order by elevation
  if (sort) {
    data <- data[order(sapply(data, get_elevation_angles))]
  }

  # prepare output
  output <- list(
    radar = radar, datetime = datetime, scans = data,
    attributes = attributes, geo = geo
  )
  class(output) <- "pvol"
  if (cleanup) {
    file.remove(file)
  }
  output
}

read_pvolfile_scan <- function(file, scan, param, radar, datetime, geo, attributes) {
  h5struct <- h5ls(file, all = TRUE)
  groups <- h5struct[h5struct$group == paste("/", scan, sep = ""), ]$name
  groups <- groups[grep("data", groups)]
  dtypes <- h5struct[startsWith(h5struct$group, paste("/", scan, "/data", sep = "")), ]
  dtypes <- dtypes[dtypes$name == "data", ]$dtype
  h5struct <- h5struct[h5struct$group == paste("/", scan, sep = ""), ]$name

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
    dtypes <- dtypes[quantityNames %in% param]
    if (length(groups) == 0) {
      return(NULL)
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
  geo$elangle <- c(attribs.where$elangle)
  geo$rscale <- c(attribs.where$rscale)
  geo$ascale <- c(360 / attribs.where$nrays)
  geo$astart <- attribs.how$astart
  # odim stores ranges as Km in package ranges are until now in meters
  geo$rstart <- attribs.where$rstart * 1000


  # read scan parameters
  quantities <- mapply(
    function(x, y) {
      read_pvolfile_quantity(
        file,
        paste(scan, "/", x, sep = ""),
        radar, datetime, geo, y
      )
    },
    x = groups,
    y = dtypes,
    SIMPLIFY = FALSE
  )
  quantityNames <- sapply(quantities, "[[", "quantityName")
  quantities <- lapply(quantities, "[[", "quantity")
  names(quantities) <- quantityNames

  # if wavelength is attribute is missing at the scan level, copy it from the pvol level
  if(is.null(attribs.how$wavelength)) attribs.how$wavelength = attributes$how$wavelength

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

read_pvolfile_quantity <- function(file, quantity, radar, datetime, geo, dtype) {
  data <- h5read(file, quantity)$data
  # convert storage mode from raw to numeric:
  storage.mode(data) <- "numeric"
  attr <- h5readAttributes(file, paste(quantity, "/what", sep = ""))
  data <- replace(data, data == as.numeric(attr$nodata), NA)
  data <- replace(data, data == as.numeric(attr$undetect), NaN)
  data <- as.numeric(attr$offset) + as.numeric(attr$gain) * data
  if(attr$quantity == "RHOHV"){
    data <- replace(data, data > 10, NaN)
  }
  conversion <- list(gain = as.numeric(attr$gain), offset = as.numeric(attr$offset),
                     nodata = as.numeric(attr$nodata), undetect = as.numeric(attr$undetect),
                     dtype = dtype)
  class(data) <- c("param", class(data))
  attributes(data)$radar <- radar
  attributes(data)$datetime <- datetime
  attributes(data)$geo <- geo
  attributes(data)$param <- as.character(attr$quantity)
  attributes(data)$conversion <- conversion
  list(quantityName = attr$quantity, quantity = data)
}
