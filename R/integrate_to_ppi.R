# helper function to calculate expected eta, vectorizing over range
eta_expected <- function(vp, quantity, distance, elev, antenna, beam_angle, k, lat, re, rp) {
  if('height' %in% names(vp$data))
  {
    height<-vp$data$height
  }else{
    height<-vp$data$HGHT
  }
  beamshapes <- t(sapply(height + vp$attributes$where$interval / 2, function(x) beam_profile(x, distance, elev, antenna = antenna, beam_angle = beam_angle, k = k, lat = lat, re = re, rp = rp)))
  if(quantity == "dens"){
    output=rcs(vp) * colSums(beamshapes * vp$data$dens, na.rm = T) / colSums(beamshapes, na.rm = T)
  }
  if(quantity == "eta"){
    output=colSums(beamshapes * vp$data$eta, na.rm = T) / colSums(beamshapes, na.rm = T)
  }
  output
}

#' calculate a range-corrected PPI
#'
#' calculates a PPI that corrects for range-effects due to partial beam overlap with
#' the layer of biological echoes (overshooting) at larger distances from the radar
#' @inheritParams integrate_to_ppi
#' @inheritParams scan_to_raster
#' @return an object of class 'scan'
#'
#' @keywords internal
#'
#' @details to be written
add_expected_eta_to_scan <- function(scan, vp, quantity="dens", param = "DBZH", lat, lon, antenna, beam_angle = 1, k = 4 / 3, re = 6378, rp = 6357) {
  if (is.null(scan$geo$height) && missing(antenna)) stop("antenna height cannot be found in scan, specify antenna height using 'antenna' argument")
  if (!(quantity %in% c("eta", "dens"))) stop(paste("quantity '", quantity, "' not one of 'eta' or 'dens'", sep = ""))
  if (!(param %in% c("DBZH", "DBZV", "DBZ", "TH", "TV"))) stop(paste(param, "not one of DBZH, DBZV, DBZ, TH, TV"))

  if (is.null(scan$geo$lat) && missing(lat)) stop("radar latitude cannot be found in polar volume, specify using 'lat' argument")
  if (is.null(scan$geo$lon) && missing(lon)) stop("radar longitude cannot be found in polar volume, specify using 'lon' argument")
  if (is.null(scan$geo$height) && missing(antenna)) stop("antenna height cannot be found in polar volume, specify antenna height using 'antenna' argument")

  if (missing(antenna)) antenna <- scan$geo$height
  assert_that(is.number(antenna))
  if (missing(lat)) lat <- scan$geo$lat
  assert_that(is.number(lat))
  if (missing(lon)) lon <- scan$geo$lon
  assert_that(is.number(lon))

  # assert that profile contains data
  if(!(FALSE %in% is.na(vp$data[quantity]))) stop(paste("input profile contains no numeric data for quantity '",quantity,"'.",sep=""))

  nazim <- dim(scan)[3]
  nrange <- dim(scan)[2]

  # reconstruct range and distance from metadata
  range <- (1:nrange) * scan$geo$rscale
  distance <- beam_distance(range, scan$geo$elangle, k = k, lat = lat, re = re, rp = rp)

  # calculate eta from reflectivity factor
  eta <- suppressWarnings(dbz_to_eta(scan$params[[param]], wavelength = vp$attributes$how$wavelength))
  attributes(eta)$param <- "eta"
  scan$params$eta <- eta

  # calculate expected_eta from beam overlap with vertical profile, either based off 'eta' or 'dens' quantity
  # that is, taking into account of thresholding by rcs_vvp_threshold ('dens') or not ('eta')
  eta_expected <- eta_expected(vp, quantity, distance, scan$geo$elangle, antenna = antenna, beam_angle = beam_angle, k = k, lat = lat, re = re, rp = rp)
  # since all azimuths are equivalent, replicate nazim times.
  eta_expected <- matrix(rep(eta_expected, nazim), nrange)
  attributes(eta_expected) <- attributes(eta)
  attributes(eta_expected)$param <- "eta_expected"
  scan$params$eta_expected <- eta_expected

  # return the scan with added scan parameters 'eta' and 'eta_expected'
  scan
}


#' calculate a range-corrected PPI
#'
#' calculates a PPI that corrects for range-effects due to partial beam overlap with
#' the layer of migration (overshooting) at larger distances from the radar
#' @inheritParams scan_to_raster
#' @inheritParams beam_profile_overlap
#' @param pvol a polar volume of class pvol
#' @param vp a vertical profile of class vp
#' @param quantity profile quantity on which to base range corrections, 'eta' or 'dens'.
#' @param param_ppi one or multiple of 'vir', 'vid', 'correction_factor', 'overlap', 'eta_sum', 'eta_sum_expected'
#' @param param reflectivity factor scan parameter on which to base range corrections.
#' Typically the same parameter from which animal densities are estimated for object \code{vp}.
#' One of 'DBZH','DBZV','DBZ','TH','TV'.
#' @param lat Geodetic latitude of the radar in degrees. If missing taken from \code{pvol}.
#' @param lon Geodetic latitude of the radar in degrees. If missing taken from \code{pvol}.
#' @return An object of class '\link[=summary.ppi]{ppi}'.
#'
#' @export
#'
#' @details TODO: DETAILS TO BE WRITTEN
#'
#' If one of \code{lat} or \code{lon} is missing, the extent of the PPI is take equal to
#' the extent of the data in the first scan of the polar volume.
#'
#' Overlap between vertical profile and vertical radiation profile is calculated using \link{beam_profile}
#' and stored as quantity 'overlap'.
#'
#' scans at 90 degree beam elevation (birdbath scans) are ignored.
#'
#' @examples
#' # locate example polar volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' # load polar volume
#' example_pvol <- read_pvolfile(pvolfile)
#' # load the corresponding vertical profile for this polar volume
#' data(example_vp)
#' # calculate the range-corrected ppi on a 100x100 pixel raster
#' my_ppi <- integrate_to_ppi(example_pvol, example_vp, nx = 100, ny = 100)
#' # plot the vertically integrated reflectivity (vir) using a 0-2000 cm^2/km^2 color scale:
#' plot(my_ppi, zlim = c(0, 2000))
#' # calculate the range-corrected ppi on finer 1000m x 1000m pixel raster:
#' my_ppi <- integrate_to_ppi(example_pvol, example_vp, res = 1000)
#' # plot the vertically integrated density (vid) using a 0-200 birds/km^2 color scale:
#' plot(my_ppi, param = "vid", zlim = c(0, 200))
#' # download a basemap, and map the ppi:
#' \dontrun{
#' bm <- download_basemap(my_ppi)
#' map(my_ppi, bm)
#' }
#' # calculate the range-corrected ppi on an even finer 500m x 500m pixel raster,
#' # cropping the area up to 50000 meter from the radar.
#' my_ppi <- integrate_to_ppi(example_pvol, example_vp,
#'   res = 500,
#'   xlim = c(-50000, 50000), ylim = c(-50000, 50000)
#' )
#' plot(my_ppi, param = "vid", zlim = c(0, 200))
integrate_to_ppi <- function(pvol, vp, nx = 100, ny = 100, xlim, ylim, zlim = c(0, 4000), res, quantity="eta",param = "DBZH", lat, lon, antenna, beam_angle = 1, crs, param_ppi = c("vir", "vid", "correction_factor", "overlap", "eta_sum", "eta_sum_expected"), k = 4 / 3, re = 6378, rp = 6357) {
  if (!is.pvol(pvol)) stop("'pvol' should be an object of class pvol")
  if (!is.vp(vp)) stop("'vp' should be an object of class vp")
  if (!is.number(nx) && missing(res)) stop("'nx' should be an integer")
  if (!is.number(ny) && missing(res)) stop("ny' should be an integer")
  if (!missing(xlim)) {
    if (length(xlim) != 2 & !is.numeric(xlim)) stop("'xlim' should be an integer vector of length two")
    if (is.na(xlim[1]) | is.na(xlim[2]) | xlim[1] > xlim[2]) stop("'xlim' should be a vector with two numeric values for upper and lower bound")
  }
  if (!missing(ylim)) {
    if (length(ylim) != 2 & !is.numeric(ylim)) stop("'ylim' should be an integer vector of length two")
    if (is.na(ylim[1]) | is.na(ylim[2]) | ylim[1] > ylim[2]) stop("'ylim' should be a vector with two numeric values for upper and lower bound")
  }
  if (!missing(zlim)) {
    if (length(zlim) != 2 & !is.numeric(zlim)) stop("'zlim' should be an integer vector of length two")
    if (is.na(zlim[1]) | is.na(zlim[2]) | zlim[1] > zlim[2]) stop("'zlim' should be a vector with two numeric values for upper and lower bound")
  }
  if (!missing(res)) {
    if(!inherits(res, 'RasterLayer')){
      assert_that(is.numeric(res))
      assert_that(length(res) <= 2)
    }
  } else {
    res <- NA
  }
  if (is.null(pvol$geo$lat) && missing(lat)) stop("radar latitude cannot be found in polar volume, specify using 'lat' argument")
  if (is.null(pvol$geo$lon) && missing(lon)) stop("radar longitude cannot be found in polar volume, specify using 'lon' argument")
  if (is.null(pvol$geo$height) && missing(antenna)) stop("antenna height cannot be found in polar volume, specify antenna height using 'antenna' argument")

  if (missing(antenna)) {
    antenna <- pvol$geo$height
  }
  assert_that(is.number(antenna))
  if (missing(lat)) lat <- pvol$geo$lat
  assert_that(is.number(lat))
  if (missing(lon)) lon <- pvol$geo$lon
  assert_that(is.number(lon))

  if (90 %in% get_elevation_angles(pvol)) {
    warning("ignoring 90 degree birdbath scan")
    pvol$scans <- pvol$scans[which(get_elevation_angles(pvol) != 90)]
  }

  # check crs argument as in raster::raster()
  if (!missing(crs)) {
    crs <- CRS(as.character(raster::projection(crs)))
  }
  else {
    crs <- NA
  }

  if (FALSE %in% (param_ppi %in% c("vir", "vid", "eta_sum", "eta_sum_expected", "azim", "range", "correction_factor", "overlap"))) stop("unknown param_ppi")
  if (!(quantity %in% c("eta", "dens"))) stop(paste("quantity '", quantity, "' not one of 'eta' or 'dens'", sep = ""))
  if (!(param %in% c("DBZH", "DBZV", "DBZ", "TH", "TV"))) stop(paste("param '", param, "' not one of DBZH, DBZV, DBZ, TH, TV", sep = ""))
  assert_that(is.number(k))
  assert_that(is.number(re))
  assert_that(is.number(rp))

  # check that request scan parameter is present in the scans of the polar volume
  param_present=sapply(pvol$scans, function(x) param %in% names(x$params))
  if(FALSE %in% param_present){
    if(TRUE %in% param_present){
      warning(paste("ignoring scan(s)",paste(which(!param_present), collapse=","),"because they have no scan parameter",param))
      pvol$scans=pvol$scans[param_present]
    }
    else{
      stop(paste("polar volume contains no scans with scan parameter ","'",param,"'",sep=""))
    }

  }

  # if extent not fully specified, determine it based off the first scan
  if(!inherits(res, 'RasterLayer'))
    if (missing(xlim) | missing(ylim)) {
      spdf <- scan_to_spatial(pvol$scans[[1]], k = k, lat = lat, lon = lon, re = re, rp = rp)
      spdf_extent <- raster::extent(spdf)
      # prepare a raster matching the data extent (or user-specified extent)
      if (missing(xlim)) xlim <- c(spdf_extent@xmin, spdf_extent@xmax)
      if (missing(ylim)) ylim <- c(spdf_extent@ymin, spdf_extent@ymax)
    }

  x <- NULL # define x to suppress devtools::check warning in next line

  if(inherits(res,'RasterLayer')){
    localCrs<- CRS(paste("+proj=aeqd +lat_0=", lat,
                       " +lon_0=", lon,
                       " +units=m",
                       sep = ""
    ))
    values(res)<-1
    spdf <- (spTransform(rasterToPoints(res,spatial=T), localCrs))
  	rasters <- lapply(pvol$scans, function(x) {
    	  scan_to_spdf(
			      add_expected_eta_to_scan(x, vp, param = param, lat = lat, lon = lon, antenna = antenna, beam_angle = beam_angle, k = k, re = re, rp = rp),
			       spdf=spdf, param = c("range", "distance", "eta", "eta_expected"),  k = k, re = re, rp = rp)
    })
    output<-as(res,'SpatialGridDataFrame')
    output@data<-rasters[[1]]@data
  }else{
  	rasters <- lapply(pvol$scans, function(x) {
    	  as(scan_to_raster(add_expected_eta_to_scan(x, vp, param = param, lat = lat, lon = lon, antenna = antenna, beam_angle = beam_angle, k = k, re = re, rp = rp), nx = nx, ny = ny, xlim = xlim, ylim = ylim, res = res, param = c("range", "distance", "eta", "eta_expected"), crs = crs, k = k, re = re, rp = rp), "SpatialGridDataFrame")
    })
    output <- rasters[[1]]
  }
  eta_expected_sum <- rowSums(do.call(cbind, lapply(1:length(rasters), function(i) (rasters[[i]]$eta_expected))), na.rm = T)
  eta_sum <- rowSums(do.call(cbind, lapply(1:length(rasters), function(i) (rasters[[i]]$eta))), na.rm = T)
  output@data$eta_sum_expected <- eta_expected_sum
  output@data$eta_sum <- eta_sum
  output@data$correction_factor <- eta_sum / eta_expected_sum
  output@data$vir <- integrate_profile(vp)$vir * eta_sum / eta_expected_sum
  output@data$vid <- integrate_profile(vp)$vid * eta_sum / eta_expected_sum

  # calculate the overlap between vp and radiated energy
  if ("overlap" %in% param_ppi) {
    # calculate overlap first for a distance grid:
    overlap <- beam_profile_overlap(vp, get_elevation_angles(pvol), seq(0, max(output@data$distance, na.rm = T), length.out = 500), antenna = antenna, zlim = zlim, steps = 500, quantity = quantity, normalize = T, beam_angle = beam_angle, k = k, lat = lat, re = re, rp = rp)
    # align our projected pixels with this distance grid:
    overlap_index <- sapply(output@data$distance, function(x) ifelse(is.na(x), NA, which.min(abs(overlap$distance - x))))
    # add the overlap data to the output
    output@data$overlap <- overlap$overlap[overlap_index]
  }

  # assemble geometry attributes
  geo <- pvol$geo
  geo$elangle <- get_elevation_angles(pvol)

  # convert the bounding box to wgs coordinates
  # geo$bbox=proj_to_wgs(output@bbox[1,],output@bbox[2,],proj4string(output))@bbox
  geo$bbox <- proj_to_wgs(output@bbox[1, ], output@bbox[2, ], proj4string = proj4string(output))@bbox
  rownames(geo$bbox) <- c("lon", "lat")
  geo$merged <- TRUE
  output_ppi <- list(radar = pvol$radar, datetime = pvol$datetime, data = output[param_ppi], geo = geo)
  class(output_ppi) <- "ppi"
  output_ppi
}
