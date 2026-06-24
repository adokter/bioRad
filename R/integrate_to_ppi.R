#' Calculate a plan position indicator (`ppi`) of vertically integrated density
#' adjusted for range effects
#'
#' Estimates a spatial image of vertically integrated density (`vid`) based on
#' all elevation scans of the radar, while accounting for the changing overlap
#' between the radar beams as a function of range. The resulting `ppi` is a
#' vertical integration over the layer of biological scatterers based on all
#' available elevation scans, corrected for range effects due to partial beam
#' overlap with the layer of biological echoes (overshooting) at larger
#' distances from the radar. The methodology is described in detail in
#' Kranstauber et al. (2020).
#'
#' @inheritParams scan_to_raster
#' @inheritParams beam_profile_overlap
#' @param pvol A `pvol` object.
#' @param vp A `vp` object
#' @param quantity Character. Profile quantity on which to base range
#'   corrections, either `eta` or `dens`.
#' @param param_ppi Character (vector). Which quantities to include in the
#' output. One or multiple of `VIR`, `VID`, `R`, `overlap`, `eta_sum`,
#' `eta_sum_expected` or `eta_sum_to_VIR`. Default includes all.
#' @param param reflectivity Character. Scan parameter on which to base range
#'   corrections. Typically the same parameter from which animal densities are
#'   estimated in `vp`. Either `DBZH`, `DBZV`, `DBZ`, `TH`, or `TV`.
#' @param lat Latitude of the radar, in degrees. If missing taken from `pvol`.
#' @param lon Latitude of the radar, in degrees. If missing taken from `pvol`.
#' @param raster (optional) `raster::RasterLayer` or `terra::SpatRaster` with a CRS. When specified
#' this raster topology is used for the output, and nx, ny, res arguments are ignored.
#' When `height_reference = "ground"` the raster values should contain
#' the ground height digital elevation in meters.
#' @param height_reference Character. Either `sea` (default) for range correction relative to
#' sea level, or `ground` for range correction relative to ground level.
#'
#' @return A `ppi` object with the following parameters:
#' * `VIR`: the vertically integrated reflectivity in cm^2/km^2
#' * `VID`: the vertically integrated density in 1/km^2
#' * `R`: the spatial adjustment factor (unitless). See Kranstauber 2020 for details.
#'   Equal to `eta_sum`/`eta_sum_expected`.
#' * `overlap`: the distribution overlap between the vertical profile `vp` and
#'    the vertical radiation profile for the set of radar sweeps in `pvol`,
#'    as calculated with [beam_profile_overlap].
#' * `eta_sum`: the sum of observed linear reflectivities over elevation angles.
#'   See Kranstauber 2020 for details.
#' * `eta_sum_expected`: the sum of expected linear reflectivities over elevation angles
#'   based on the input vertical profile `vp`. See Kranstauber 2020 for details.
#' * `eta_sum_to_VIR`: the multiplicative factor for converting the sum of expected
#'   linear reflectivities (`eta_sum_expected`) to vertically integrated reflectivity (`VIR`).
#'   Identical to `integrate_profile(vp)$vir/eta_sum_expected`. See Kranstauber 2020 for details.
#' @family ppi calculation functions
#' @export
#'
#' @details
#' The function requires:
#'
#' * A polar volume, containing one or multiple scans (`pvol`).
#' * A vertical profile (of birds) calculated for that same polar volume (`vp`).
#' * A grid defined on the earth's surface, on which we will calculate the range
#' corrected image (defined by `raster`, or a combination of `nx`, `ny`,`res`
#' arguments).
#'
#' The pixel locations on the ground are easily translated into a corresponding
#' azimuth and range of the various scans (see [beam_range()]).
#'
#' For each scan within the polar volume, the function calculates:
#' * the vertical radiation profile for each ground surface pixel for that
#'   particular scan, using [beam_profile].
#' * the reflectivity expected for each ground surface pixel
#'   (\eqn{\eta_{expected}}), given the vertical profile (of biological
#'   scatterers) and the part of the profile radiated by the beam.
#'   This \eqn{\eta_{expected}} is simply the average of (linear) `eta` in the
#'   profile, weighted by the vertical radiation profile.
#' * the observed eta at each pixel \eqn{\eta_{observed}}, which is converted
#'   form `DBZH` using function [dbz_to_eta], with `DBZH` the reflectivity
#'   factor measured at the pixel's distance from the radar.
#'
#' If one of `lat` or `lon` is missing, the extent of the `ppi` is taken equal
#' to the extent of the data in the first scan of the polar volume.
#'
#' To arrive at the final PPI image, the function calculates
#' * the vertically integrated density (`vid`) and vertically integrated
#'   reflectivity (`vir`) for the profile, using the function
#'   [integrate_profile].
#' * the spatial range-corrected PPI for `VID`, defined as the adjustment
#'   factor image (`R`), multiplied by the `vid` calculated for the profile
#' * the spatial range-corrected PPI for `VIR`, defined as the adjustment
#'   factor `R`, multiplied by the `vir` calculated for the profile.
#'
#' Scans at 90 degree beam elevation (e.g. birdbath scans) are ignored.
#'
#' @references
#' * Kranstauber B, Bouten W, Leijnse H, Wijers B, Verlinden L, Shamoun-Baranes
#' J, Dokter AM (2020) High-Resolution Spatial Distribution of Bird Movements
#' Estimated from a Weather Radar Network. Remote Sensing 12 (4), 635.
#' \doi{https://doi.org/10.3390/rs12040635}
#' * Buler JJ & Diehl RH (2009) Quantifying bird density during migratory
#' stopover using weather surveillance radar. IEEE Transactions on Geoscience
#' and Remote Sensing 47: 2741-2751.
#' \doi{https://doi.org/10.1109/TGRS.2009.2014463}
#'
#' @examples
#' \donttest{
#' # Locate and read the polar volume example file
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#'
#' # load polar volume
#' pvol <- read_pvolfile(pvolfile)
#'
#' # Read the corresponding vertical profile example
#' data(example_vp)
#'
#' # Calculate the range-corrected ppi on a 50x50 pixel raster
#' ppi <- integrate_to_ppi(pvol, example_vp, nx = 50, ny = 50)
#'
#' # Plot the vertically integrated reflectivity (VIR) using a
#' # 0-2000 cm^2/km^2 color scale
#' plot(ppi, zlim = c(0, 2000))
#'
#' # Calculate the range-corrected ppi on finer 2000m x 2000m pixel raster
#' ppi <- integrate_to_ppi(pvol, example_vp, res = 2000)
#'
#' # Plot the vertically integrated density (VID) using a
#' # 0-200 birds/km^2 color scale
#' plot(ppi, param = "VID", zlim = c(0, 200))
#'
#' # Download a basemap and map the ppi
#' if (all(sapply(c("ggspatial","prettymapr", "rosm"), requireNamespace, quietly = TRUE))) {
#' map(ppi)
#' # First define the raster
#' template_raster <- raster::raster(
#'   raster::extent(12, 13, 56, 57),
#'   crs = sp::CRS("+proj=longlat")
#' )
#'
#' # Project the ppi on the defined raster
#' ppi <- integrate_to_ppi(pvol, example_vp, raster = template_raster)
#'
#' # Extract the raster data from the ppi object
#' raster::brick(ppi$data)
#'
#' # Calculate the range-corrected ppi on an even finer 500m x 500m pixel raster,
#' # cropping the area up to 50000 meter from the radar
#' ppi <- integrate_to_ppi(
#'   pvol, example_vp, res = 500,
#'   xlim = c(-50000, 50000), ylim = c(-50000, 50000)
#' )
#' plot(ppi, param = "VID", zlim = c(0, 200))
#'
#' # To calculate a range-corrected map assuming a constant altitude
#' # profile maintained relative to ground height:
#' if(requireNamespace("elevatr", quietly = TRUE)){
#'
#' # define a radar-centred grid (azimuthal equidistant, radar at the center):
#' pvol |>
#'   get_scan(.5) |>
#'   scan_to_raster(param = "DBZH") |>
#'   terra::rast() -> radar_grid
#'
#' # download elevation data and resample onto that grid. `expand`
#' # over-requests so the full grid is covered;
#' data_dem <- terra::project(
#'   terra::rast(elevatr::get_elev_raster(radar_grid, z = 5, expand = 100000)), radar_grid)
#' # set heights below sea level to zero.
#' data_dem[data_dem < 0] <- 0
#' # add an informative name
#' names(data_dem) <- "HGHT"
#'
#' # add DEM data to the polar volume:
#' pvol_ground <- add_param(pvol, data_dem, "HGHT")
#' # compute a profile relative to ground:
#' vp_ground   <- calculate_vp(pvol_ground, n_layer = 60, h_layer = 50, height_reference = "ground")
#' # apply the range correction:
#' ppi_ground  <- integrate_to_ppi(pvol_ground, vp_ground, height_reference = "ground",
#'                                 raster = data_dem)
#' }
#' }
#' }
#' @references
#' * Kranstauber B, Bouten W, Leijnse H, Wijers B, Verlinden L, Shamoun-Baranes
#'   J, Dokter AM (2020) High-Resolution Spatial Distribution of Bird
#'   Movements Estimated from a Weather Radar Network. Remote Sensing 12 (4),
#'   635. \doi{10.3390/rs12040635}
#' * Buler JJ & Diehl RH (2009) Quantifying bird density during migratory
#'   stopover using weather surveillance radar. IEEE Transactions on Geoscience
#'   and Remote Sensing 47: 2741-2751.
#'   \doi{10.1109/TGRS.2009.2014463}
integrate_to_ppi <- function(pvol, vp, nx = 100, ny = 100, xlim, ylim, zlim = c(0, 4000), res, quantity = "eta", param = "DBZH", raster = NA, lat, lon, antenna, beam_angle = 1, crs, param_ppi = c("VIR", "VID", "R", "overlap", "eta_sum", "eta_sum_expected", "eta_sum_to_VIR"), k = 4 / 3, re = 6378, rp = 6357, height_reference="sea") {
  if (!is.pvol(pvol)) stop("'pvol' should be an object of class pvol")
  if (!is.vp(vp)) stop("'vp' should be an object of class vp")
  if (!assertthat::is.number(nx) && missing(res)) stop("'nx' should be an integer")
  if (!assertthat::is.number(ny) && missing(res)) stop("'ny' should be an integer")
  if (!missing(xlim)) {
    assertthat::assert_that(
      is.numeric(xlim),
      msg = "'xlim' should be an integer vector of length two")
    assertthat::assert_that(
      length(xlim) == 2,
      msg = "'xlim' should be an integer vector of length two")
    assertthat::assert_that(
      all(!is.na(xlim)),
      msg = "'xlim' should be a vector with two numeric values for upper and lower bound")
    assertthat::assert_that(
      xlim[1] != xlim[2],
      msg = "upper and lower bound of `xlim` can not be identical")
    assertthat::assert_that(
      xlim[1] < xlim [2],
      msg = "'xlim' should be a vector with two numeric values for upper and lower bound")
  }
  if (!missing(ylim)) {
    assertthat::assert_that(
      is.numeric(ylim),
      msg = "'ylim' should be an integer vector of length two")
    assertthat::assert_that(
      length(ylim) == 2,
      msg = "'ylim' should be an integer vector of length two"
    )
    assertthat::assert_that(
      all(!is.na(ylim)),
      msg = "'ylim' should be a vector with two numeric values for upper and lower bound"
    )
    assertthat::assert_that(
      ylim[1] != ylim[2],
      msg = "upper and lower bound of `ylim` can not be identical")
    assertthat::assert_that(
      ylim[1] < ylim[2],
      msg = "'ylim' should be a vector with two numeric values for upper and lower bound"
    )
  }
  if (!missing(zlim)) {
    assertthat::assert_that(
      is.numeric(zlim),
      msg = "'zlim' should be an integer vector of length two")
    assertthat::assert_that(
      length(zlim) == 2,
      msg = "'zlim' should be an integer vector of length two"
    )
    assertthat::assert_that(
      all(!is.na(zlim)),
      msg = "'zlim' should be a vector with two numeric values for upper and lower bound"
    )
    assertthat::assert_that(
      zlim[1] != zlim[2],
      msg = "upper and lower bound of `zlim` can not be identical")
    assertthat::assert_that(
      zlim[1] < zlim[2],
      msg = "'zlim' should be a vector with two numeric values for upper and lower bound"
    )
  }
  if (!missing(res)) {
    assertthat::assert_that(is.numeric(res))
    assertthat::assert_that(length(res) <= 2)
  } else {
    res <- NA
  }
  # accept a terra SpatRaster by converting it to a raster::RasterLayer
  if (inherits(raster, "SpatRaster")) raster <- raster::raster(raster)
  if (!assertthat::are_equal(raster, NA)) {
    assertthat::assert_that(inherits(raster, "RasterLayer"))
  }
  if (is.null(pvol$geo$lat) && missing(lat)) stop("radar latitude cannot be found in polar volume, specify using 'lat' argument")
  if (is.null(pvol$geo$lon) && missing(lon)) stop("radar longitude cannot be found in polar volume, specify using 'lon' argument")
  if (is.null(pvol$geo$height) && missing(antenna)) stop("antenna height cannot be found in polar volume, specify antenna height using 'antenna' argument")

  if (missing(antenna)) {
    antenna <- pvol$geo$height
  }
  assertthat::assert_that(assertthat::is.number(antenna))
  if (missing(lat)) lat <- pvol$geo$lat
  assertthat::assert_that(assertthat::is.number(lat))
  if (missing(lon)) lon <- pvol$geo$lon
  assertthat::assert_that(assertthat::is.number(lon))

  if (90 %in% get_elevation_angles(pvol)) {
    warning("ignoring 90 degree birdbath scan")
    pvol$scans <- pvol$scans[which(get_elevation_angles(pvol) != 90)]
  }

  # check crs argument as in raster::raster()
  if (!missing(crs)) {
    crs <- sp::CRS(as.character(raster::projection(crs)))
  }
  else {
    crs <- NA
  }

  if (FALSE %in% (param_ppi %in% c("VIR", "VID", "eta_sum", "eta_sum_expected","eta_sum_to_VIR", "azim", "range", "R", "overlap"))) stop("unknown param_ppi")
  assertthat::assert_that(length(quantity) == 1)
  if (!(quantity %in% c("eta", "dens"))) stop(paste("quantity '", quantity, "' not one of 'eta' or 'dens'", sep = ""))
  allowed_params <- c("DBZH", "DBZV", "DBZ", "TH", "TV")
  assertthat::assert_that(
    all(param %in% allowed_params),
    msg = glue::glue(
      "param {param_collapse} not one of DBZH, DBZV, DBZ, TH or TV",
      param_collapse =
        glue::glue_collapse(glue::backtick(param[!param %in% allowed_params]),
                            sep = ", ",
                            last = " & ")
      )
    )
  assertthat::assert_that(assertthat::is.number(k))
  assertthat::assert_that(assertthat::is.number(re))
  assertthat::assert_that(assertthat::is.number(rp))

  height_reference <- rlang::arg_match(height_reference,c("sea","antenna", "ground"))
  if(height_reference == "ground"){
    assertthat::assert_that(inherits(raster,"RasterLayer"),
    msg="For estimations relative to ground level (`height_reference = \"ground\"`),
    provide a digital elevation map using argument `raster`")
  }

  # make sure height reference of vp matches requested height_reference
  if(height_reference != determine_height_reference(vp)){
      warning(paste0("Height reference of `vp` (", determine_height_reference(vp), ") does not match `height_reference` (", height_reference,"), results may be unreliable."))
  }

  # check that request scan parameter is present in the scans of the polar volume
  param_present <- sapply(pvol$scans, function(x) param %in% names(x$params))
  if (FALSE %in% param_present) {
    if (TRUE %in% param_present) {
      warning(paste("ignoring scan(s)", paste(which(!param_present), collapse = ","), "because they have no scan parameter", param))
      pvol$scans <- pvol$scans[param_present]
    }
    else {
      stop(paste("polar volume contains no scans with scan parameter ", "'", param, "'", sep = ""))
    }
  }

  # if extent not fully specified, determine it based off the first scan
  if (assertthat::are_equal(raster, NA)) {
    if (missing(xlim) | missing(ylim)) {
      spdf <- scan_to_spatial(pvol$scans[[1]], k = k, lat = lat, lon = lon, re = re, rp = rp)
      spdf_extent <- raster::extent(spdf)
      # prepare a raster matching the data extent (or user-specified extent)
      if (missing(xlim)) xlim <- c(spdf_extent@xmin, spdf_extent@xmax)
      if (missing(ylim)) ylim <- c(spdf_extent@ymin, spdf_extent@ymax)
    }
  }

  x <- NULL # define x to suppress devtools::check warning in next line

  if (!assertthat::are_equal(raster, NA)) {
    localCrs <- paste("+proj=aeqd +lat_0=", lat,
      " +lon_0=", lon,
      " +units=m",
      sep = ""
    )

    # add ground height level as a scan parameter:
    if(height_reference == "ground"){
      pvol = add_param(pvol, raster, "HGHT")
    }

    # set NA values to zero (note: used to be set to 1)
    # required for next statement to work
    raster::values(raster)[is.na(raster::values(raster))] <- 0

    spdf<-as(sf::as_Spatial(
      sf::st_transform(sf::st_as_sf(as.data.frame(raster::rasterToPoints(raster)), coords=c("x","y"),
                                                          crs=sf::st_crs(raster)), sf::st_crs(localCrs))),"SpatialPointsDataFrame")

    rasters <- lapply(pvol$scans, function(x) {
      scan_to_spdf(
        add_expected_eta_to_scan(x, vp, param = param, lat = lat, lon = lon, antenna = antenna, beam_angle = beam_angle, k = k, re = re, rp = rp, height_reference = height_reference),
        spdf = spdf, param = c("range", "distance", "eta", "eta_expected"), k = k, re = re, rp = rp
      )
    })
    output <- methods::as(raster, "SpatialGridDataFrame")
    output@data <- rasters[[1]]@data
  } else {
    rasters <- lapply(pvol$scans, function(x) {
      methods::as(scan_to_raster(add_expected_eta_to_scan(x, vp, param = param, lat = lat, lon = lon, antenna = antenna, beam_angle = beam_angle, k = k, re = re, rp = rp, height_reference = height_reference), nx = nx, ny = ny, xlim = xlim, ylim = ylim, res = res, param = c("range", "distance", "eta", "eta_expected"), raster = raster, crs = crs, k = k, re = re, rp = rp), "SpatialGridDataFrame")
    })
    output <- rasters[[1]]
  }
  eta_expected_sum <-
    rowSums(do.call(cbind, lapply(1:length(rasters), function(i) (rasters[[i]]$eta_expected))), na.rm = TRUE)
  eta_sum <-
    rowSums(do.call(cbind, lapply(1:length(rasters), function(i) (rasters[[i]]$eta))), na.rm = TRUE)
  output@data$eta_sum_expected <- eta_expected_sum
  output@data$eta_sum <- eta_sum
  output@data$R <- eta_sum / eta_expected_sum
  output@data$VIR <- integrate_profile(vp, alt_min=zlim[1], alt_max=zlim[2])$vir * eta_sum / eta_expected_sum
  output@data$VID <- integrate_profile(vp, alt_min=zlim[1], alt_max=zlim[2])$vid * eta_sum / eta_expected_sum
  output@data$eta_sum_to_VIR <- integrate_profile(vp, alt_min=zlim[1], alt_max=zlim[2])$vir / eta_expected_sum

  # calculate the overlap between vp and radiated energy
  if ("overlap" %in% param_ppi) {
    # calculate overlap first for a distance grid:
    overlap <-
      beam_profile_overlap(
        vp,
        get_elevation_angles(pvol),
        seq(0, max(output@data$distance, na.rm = TRUE), length.out = 500),
        antenna = antenna,
        zlim = zlim,
        steps = 500,
        quantity = quantity,
        normalize = TRUE,
        beam_angle = beam_angle,
        k = k,
        lat = lat,
        re = re,
        rp = rp,
        path = "two_way"
      )
    # align our projected pixels with this distance grid:
    overlap_index <-
      sapply(output@data$distance, function(x)
        ifelse(is.na(x), NA, which.min(abs(overlap$distance - x))))
    # add the overlap data to the output
    output@data$overlap <- overlap$overlap[overlap_index]
  }

  # assemble geometry attributes
  geo <- pvol$geo
  geo$elangle <- get_elevation_angles(pvol)

  # convert the bounding box to wgs coordinates
  # geo$bbox=proj_to_wgs(output@bbox[1,],output@bbox[2,],sp::proj4string(output))@bbox
  geo$bbox <- proj_to_wgs(output@bbox[1, ], output@bbox[2, ], proj4string = sp::proj4string(output))@bbox
  rownames(geo$bbox) <- c("lon", "lat")
  geo$merged <- TRUE
  output_ppi <-
    list(radar = pvol$radar,
         datetime = pvol$datetime,
         data = output[param_ppi], geo = geo)
  class(output_ppi) <- "ppi"
  output_ppi
}

# Helper function to extract height reference from metadata or data
determine_height_reference <- function(vp){
  height_ref <- vp$attributes$how$height_reference
  if(is.null(height_ref)){
    height_ref <- vp$data$height_reference[1]
  }
  if(is.null(height_ref)) height_ref <- "sea"
  return(height_ref)
}

# Helper function to calculate expected eta, vectorizing over range
eta_expected <- function(vp,
                         quantity,
                         distance,
                         elev,
                         antenna,
                         beam_angle,
                         k,
                         lat, re, rp) {
  beamshapes <-
    t(sapply(
      vp$data$height + vp$attributes$where$interval / 2,
      function(x) {
        beam_profile(x,
          distance,
          elev,
          antenna = antenna,
          beam_angle = beam_angle,
          k = k, lat = lat, re = re, rp = rp, path = "two_way"
        )
      }
    ))

  if (quantity == "dens") {
    output <-
      rcs(vp) * colSums(beamshapes * vp$data$dens,
                              na.rm = TRUE) / colSums(beamshapes, na.rm = TRUE)
  }
  if (quantity == "eta") {
    output <-
      colSums(beamshapes * vp$data$eta, na.rm = TRUE) / colSums(beamshapes,
                                                             na.rm = TRUE)
  }
  output
}

#' Adds expected eta to a scan
#'
#' @inheritParams integrate_to_ppi
#' @inheritParams scan_to_raster
#' @return A `scan` object.
#'
#' @keywords internal
#'
add_expected_eta_to_scan <- function(scan, vp, quantity = "dens",
                                     param = "DBZH", lat, lon, antenna,
                                     beam_angle = 1, k = 4 / 3, re = 6378,
                                     rp = 6357, height_reference = "sea") {

  if (is.null(scan$geo$height) &&
      missing(antenna)) {
    stop(
      paste("antenna height cannot be found in scan,",
            "specify antenna height using 'antenna' argument")
    )
  }
  if (!(quantity %in% c("eta", "dens"))) {
    stop(
      paste("quantity '", quantity, "' not one of 'eta' or 'dens'", sep = "")
      )
  }
  if (!(param %in% c("DBZH", "DBZV", "DBZ", "TH", "TV"))) {
    stop(paste(param, "not one of DBZH, DBZV, DBZ, TH, TV"))
  }

  if (is.null(scan$geo$lat) && missing(lat)) {
    stop(paste("radar latitude cannot be found in polar volume,",
               "specify using 'lat' argument"))}
  if (is.null(scan$geo$lon) &&
      missing(lon)) {
    stop(paste("radar longitude cannot be found in polar volume,",
               "specify using 'lon' argument"))
  }

  if (missing(antenna)) antenna <- scan$geo$height
  assertthat::assert_that(assertthat::is.number(antenna))
  if (missing(lat)) lat <- scan$geo$lat
  assertthat::assert_that(assertthat::is.number(lat))
  if (missing(lon)) lon <- scan$geo$lon
  assertthat::assert_that(assertthat::is.number(lon))

  # assert that profile contains data
  if (!(FALSE %in% is.na(vp$data[quantity]))) {
    stop(paste(
      "input profile contains no numeric data for quantity '",
      quantity,
      "'.",
      sep = ""
    ))
  }

  nazim <- dim(scan)[3]
  nrange <- dim(scan)[2]

  # reconstruct range and distance from metadata
  range <- (1:nrange) * scan$geo$rscale
  distance <-
    beam_distance(
      range,
      scan$geo$elangle,
      k = k,
      lat = lat,
      re = re,
      rp = rp
    )

  # calculate eta from reflectivity factor
  eta <-
     suppressWarnings(
       dbz_to_eta(scan$params[[param]],
                  wavelength = vp$attributes$how$wavelength))
  attributes(eta)$param <- "eta"
  scan$params$eta <- eta

  # calculate the effective antenna height, relative to ground, sea or antenna level:
  if(height_reference == "ground"){
    distance <- rep(distance, nazim)
    effective_antenna <- antenna - c(scan$params$HGHT)
  }
  else if(height_reference == "antenna"){
    effective_antenna <- 0
  } else{
    # height_reference == "sea"
    effective_antenna <- antenna
  }

  # calculate expected_eta from beam overlap with vertical profile, either based
  # off 'eta' or 'dens' quantity that is, taking into account of thresholding by
  # rcs_vvp_threshold ('dens') or not ('eta')
  eta_expected <-
    eta_expected(
      vp,
      quantity,
      distance,
      scan$geo$elangle,
      antenna = effective_antenna,
      beam_angle = beam_angle,
      k = k,
      lat = lat,
      re = re,
      rp = rp
    )

  # since all azimuths are equivalent, replicate nazim times.
  if(height_reference == "sea" || height_reference == "antenna") eta_expected <- rep(eta_expected, nazim)
  eta_expected <- matrix(eta_expected, nrange)
  attributes(eta_expected) <- attributes(eta)
  attributes(eta_expected)$param <- "eta_expected"
  scan$params$eta_expected <- eta_expected

  # set eta_expected values to zero whenever the reflectivity quantity is NA
  # NA values indicate the pixel was never irradiated, so no reflectivity return expected
  na_idx <- is.na(scan$params[[param]]) & !is.nan(scan$params[[param]])
  scan$params[["eta_expected"]][na_idx] <- 0

  # return the scan with added scan parameters 'eta' and 'eta_expected'
  scan
}
