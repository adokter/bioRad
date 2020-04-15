pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)
scan <- pvol$scans[[1]]
param <- scan$params$DBZH

test_that("returns error on incorrect parameters", {
  expect_error(get_elevation_angles("not_a_vp"))
  expect_error(get_elevation_angles(1))
  expect_error(get_elevation_angles(NULL))
})

test_that("get_elevation_angles returns correct elevation angle", {
  expect_equal(get_elevation_angles(scan), scan$attributes$where$elangle)
  expect_equal(get_elevation_angles(param), attributes(param)$geo$elangle)
})

test_that("get_elevation_angles returns expected values", {
  expect_type(get_elevation_angles(scan), "double")
  expect_type(get_elevation_angles(pvol), "double")
  expect_length(get_elevation_angles(pvol), length(pvol$scans))
  expect_type(get_elevation_angles(param), "double")
  expect_true(all(get_elevation_angles(pvol) >= 0, na.rm = TRUE))
  expect_true(all(get_elevation_angles(pvol) <= 90, na.rm = TRUE))
})
