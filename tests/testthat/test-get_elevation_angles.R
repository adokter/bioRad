pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)
scan <- get_scan(pvol, 0.5)
param <- get_param(scan, "DBZH")

test_that("get_elevation_angles() returns error on incorrect parameters", {
  expect_error(get_elevation_angles("not_a_vp"))
})

test_that("get_elevation_angles() returns the correct elangle", {
  pvol_elangles <- c()
  for (scan in pvol$scans) {
    pvol_elangles <- c(pvol_elangles, scan$attributes$where$elangle)
  }

  expect_equal(get_elevation_angles(pvol), pvol_elangles)
  expect_equal(get_elevation_angles(scan), scan$attributes$where$elangle)
  expect_equal(get_elevation_angles(param), attributes(param)$geo$elangle)
})

test_that("get_elevation_angles() returns a double", {
  expect_type(get_elevation_angles(pvol), "double") # Vector of doubles
  expect_type(get_elevation_angles(scan), "double")
  expect_type(get_elevation_angles(param), "double")
})

test_that("elevation angle values are between 0 and 90", {
  expect_true(all(get_elevation_angles(pvol) >= 0, na.rm = TRUE))
  expect_true(all(get_elevation_angles(pvol) <= 90, na.rm = TRUE))
})

test_that("get_elevation_angles() returns the same number of angles as scans", {
  expect_length(get_elevation_angles(pvol), length(pvol$scans))
})
