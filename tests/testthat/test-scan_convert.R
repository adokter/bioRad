test_that("legacy scan conversion functions are deprecated", {
  points <- sf::as_Spatial(scan_to_sf(example_scan))

  lifecycle::expect_deprecated(scan_to_spatial(example_scan))
  lifecycle::expect_deprecated(scan_to_raster(example_scan, nx = 10, ny = 10))
  lifecycle::expect_deprecated(scan_to_spdf(example_scan, points, param = "DBZH"))
})

test_that("scan_to_sf() returns error on incorrect parameters", {
  expect_error(scan_to_sf(scan = "a"),
               regexp = "is.scan(x = scan) is not TRUE",
               fixed = TRUE)
  expect_error(scan_to_sf(example_scan, k = "a"),
               regexp = "k is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(scan_to_sf(example_scan, re = "a"),
               regexp = "re is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(scan_to_sf(example_scan, rp = "a"),
               regexp = "rp is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(scan_to_sf(example_scan, lat = "a"),
               regexp = "lat is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(scan_to_sf(example_scan, lon = "a"),
               regexp = "lon is not a number (a length one numeric vector).",
               fixed = TRUE)
  missing_lat_scan <- example_scan
  missing_lon_scan <- example_scan
  missing_lat_scan$geo$lat <- NULL
  missing_lon_scan$geo$lon <- NULL
  expect_error(scan_to_sf(missing_lat_scan),
               regexp = "radar latitude cannot be found in scan, specify using 'lat' argument",
               fixed = TRUE)
  expect_error(scan_to_sf(missing_lon_scan),
               regexp = "radar longitude cannot be found in scan, specify using 'lon' argument",
               fixed = TRUE)
})

test_that("scan_to_spatraster() returns error on incorrect parameters", {
  expect_error(scan_to_spatraster("a"),
               regexp = "'scan' should be an object of class scan",
               fixed = TRUE)
  square_birdbath <- example_scan
  square_birdbath$attributes$where$elangle <- 90
  expect_error(scan_to_spatraster(square_birdbath),
               regexp = "georeferencing of 90 degree birdbath scan not supported",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, nx = "a"),
               regexp = "'nx' should be an integer",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, ny = "a"),
               regexp = "'ny' should be an integer",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, xlim = "a"),
               regexp = "'xlim' should be an integer vector of length two",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, xlim = 2),
               regexp = "'xlim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, xlim = c("a",2)),
               regexp = "'xlim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, xlim = c(pi, 2)),
               regexp = "'xlim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, ylim = "a"),
               regexp = "'ylim' should be an integer vector of length two",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, ylim = 2),
               regexp = "'ylim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, ylim = c("a",2)),
               regexp = "'ylim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, ylim = c(pi, 2)),
               regexp = "'ylim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  missing_lat_scan <- example_scan
  missing_lon_scan <- example_scan
  missing_lat_scan$geo$lat <- NULL
  missing_lon_scan$geo$lon <- NULL
  expect_error(scan_to_spatraster(missing_lat_scan),
               regexp = "radar latitude cannot be found in scan, specify using 'lat' argument",
               fixed = TRUE)
  expect_error(scan_to_spatraster(missing_lon_scan),
               regexp = "radar longitude cannot be found in scan, specify using 'lon' argument",
               fixed = TRUE)
})

test_that("scan_to_spatraster() returns error on wrongly formed param argument",{
  expect_error(scan_to_spatraster(example_scan, param = "unexisting_param"),
               regexp = "'param' contains scan parameter not found in scan",
               fixed = TRUE)
  expect_error(scan_to_spatraster(example_scan, param = "azim"),
               regexp = "'param' should contain the name of one or more scan parameters contained in 'scan'",
               fixed = TRUE)
})

test_that("scan_to_spatraster() raster argument produces expected raster output", {
  data(example_vpts)
  expect_s4_class(b <- scan_to_spatraster(example_scan, ylim = c(55, 57), xlim = c(12, 13), res = .1), "SpatRaster")
  from_template <- scan_to_spatraster(example_scan, raster = b)
  expect_true(terra::compareGeom(b, from_template, stopOnError = FALSE))
  expect_equal(terra::values(b), terra::values(from_template))
})

test_that("scan_to_spatraster() accepts raster templates", {
  template <- raster::raster(scan_to_spatraster(example_scan, ylim = c(55, 57), xlim = c(12, 13), res = .1))
  # a terra SpatRaster template must produce the same output as the equivalent RasterLayer
  from_terra <- scan_to_spatraster(example_scan, raster = terra::rast(template))
  from_raster <- scan_to_spatraster(example_scan, raster = template)
  expect_true(terra::compareGeom(from_terra, from_raster, stopOnError = FALSE))
  expect_equal(terra::values(from_terra), terra::values(from_raster))
})

test_that("new scan conversions preserve legacy output", {
  modern_points <- scan_to_sf(example_scan)
  legacy_points <- suppressWarnings(scan_to_spatial(example_scan))

  expect_equal(sf::st_drop_geometry(modern_points), legacy_points@data)
  expect_equal(
    unname(sf::st_coordinates(modern_points)),
    unname(sp::coordinates(legacy_points))
  )
  expect_true(sf::st_crs(modern_points) == sf::st_crs(legacy_points))

  modern_raster <- scan_to_spatraster(example_scan, nx = 20, ny = 20)
  legacy_raster <- suppressWarnings(scan_to_raster(example_scan, nx = 20, ny = 20))
  expect_equal(names(modern_raster), names(legacy_raster))
  expect_equal(terra::values(modern_raster), raster::values(legacy_raster))
  expect_equal(unname(as.vector(terra::ext(modern_raster))), unname(as.vector(raster::extent(legacy_raster))))
  expect_equal(terra::res(modern_raster), raster::res(legacy_raster))
  expect_true(sf::st_crs(modern_raster) == sf::st_crs(legacy_raster))
})

test_that(".scan_to_spdf() returns error error on incorrect parameters",{
  expect_error(bioRad:::.scan_to_spdf("a"),
               regexp = "'scan' should be an object of class scan",
               fixed = TRUE)
  square_birdbath <- example_scan
  square_birdbath$attributes$where$elangle <- 90
  expect_error(bioRad:::.scan_to_spdf(square_birdbath),
               regexp = "georeferencing of 90 degree birdbath scan not supported",
               fixed = TRUE)
})

test_that(".scan_to_spdf() returns error on wrongly formed param argument",{
  points <- sf::as_Spatial(scan_to_sf(example_scan))
  expect_error(bioRad:::.scan_to_spdf(example_scan, param = "unexisting_param",
                            spdf = points),
               regexp = "'param' contains scan parameter not found in scan",
               fixed = TRUE)
  expect_error(bioRad:::.scan_to_spdf(example_scan, param = "azim",
                            spdf = points),
               regexp = "'param' should contain the name of one or more scan parameters contained in 'scan'",
               fixed = TRUE)
  expect_error(bioRad:::.scan_to_spdf(example_scan),
               regexp = 'argument "spdf" is missing, with no default',
               fixed = TRUE)
  expect_error(
    bioRad:::.scan_to_spdf(example_scan,
                 points,
                 lat = "a"),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    bioRad:::.scan_to_spdf(example_scan,
                 points,
                 lon = "a"),
    regexp = "lon is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  missing_lat_scan <- example_scan
  missing_lon_scan <- example_scan
  missing_lat_scan$geo$lat <- NULL
  missing_lon_scan$geo$lon <- NULL
  expect_error(
    bioRad:::.scan_to_spdf(missing_lat_scan, points),
    regexp = "radar latitude cannot be found in scan, specify using 'lat' argument",
    fixed = TRUE
  )
  expect_error(
    bioRad:::.scan_to_spdf(missing_lon_scan, points),
    regexp = "radar longitude cannot be found in scan, specify using 'lon' argument",
    fixed = TRUE
  )
  expect_error(
    bioRad:::.scan_to_spdf(example_scan, points, k = "a"),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    bioRad:::.scan_to_spdf(example_scan, points, re = "a"),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    bioRad:::.scan_to_spdf(example_scan, points, rp = "a"),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  # TODO add coverage for crs mismatch between local crs and spdf crs
})
