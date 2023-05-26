test_that("scan_to_spatial() returns error on incorrect parameters", {
  expect_error(scan_to_spatial(scan = "a"),
               regexp = "is.scan(x = scan) is not TRUE",
               fixed = TRUE)
  expect_error(scan_to_spatial(example_scan, k = "a"),
               regexp = "k is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(scan_to_spatial(example_scan, re = "a"),
               regexp = "re is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(scan_to_spatial(example_scan, rp = "a"),
               regexp = "rp is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(scan_to_spatial(example_scan, lat = "a"),
               regexp = "lat is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(scan_to_spatial(example_scan, lon = "a"),
               regexp = "lon is not a number (a length one numeric vector).",
               fixed = TRUE)
  missing_lat_scan <- example_scan
  missing_lon_scan <- example_scan
  purrr::pluck(missing_lat_scan,"geo","lat") <- NULL
  purrr::pluck(missing_lon_scan,"geo","lon") <- NULL
  expect_error(scan_to_spatial(missing_lat_scan),
               regexp = "radar latitude cannot be found in scan, specify using 'lat' argument",
               fixed = TRUE)
  expect_error(scan_to_spatial(missing_lon_scan),
               regexp = "radar longitude cannot be found in scan, specify using 'lon' argument",
               fixed = TRUE)
})

test_that("scan_to_raster() returns error on incorrect parameters", {
  expect_error(scan_to_raster("a"),
               regexp = "'scan' should be an object of class scan",
               fixed = TRUE)
  square_birdbath <- example_scan
  purrr::pluck(square_birdbath,"attributes","where","elangle") <- 90
  expect_error(scan_to_raster(square_birdbath),
               regexp = "georeferencing of 90 degree birdbath scan not supported",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, nx = "a"),
               regexp = "'nx' should be an integer",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, ny = "a"),
               regexp = "'ny' should be an integer",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, xlim = "a"),
               regexp = "'xlim' should be an integer vector of length two",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, xlim = 2),
               regexp = "'xlim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, xlim = c("a",2)),
               regexp = "'xlim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, xlim = c(pi, 2)),
               regexp = "'xlim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, ylim = "a"),
               regexp = "'ylim' should be an integer vector of length two",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, ylim = 2),
               regexp = "'ylim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, ylim = c("a",2)),
               regexp = "'ylim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, ylim = c(pi, 2)),
               regexp = "'ylim' should be a vector with two numeric values for upper and lower bound",
               fixed = TRUE)

})

test_that("scan_to_raster() returns error on wrongly formed param argument",{
  expect_error(scan_to_raster(example_scan, param = "unexisting_param"),
               regexp = "'param' contains scan parameter not found in scan",
               fixed = TRUE)
  expect_error(scan_to_raster(example_scan, param = "azim"),
               regexp = "'param' should contain the name of one or more scan parameters contained in 'scan'",
               fixed = TRUE)
})

test_that("scan_to_raster() raster argument produces expected raster output", {
  data(example_vpts)
  expect_s4_class(b <- scan_to_raster(example_scan, ylim = c(55, 57), xlim = c(12, 13), res = .1), "RasterBrick")
  expect_equal(b, scan_to_raster(example_scan, raster = raster(b)))
})
