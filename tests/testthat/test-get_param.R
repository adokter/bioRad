pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)
scan <- pvol$scans[[1]]

test_that("returns error on incorrect parameters", {
  expect_error(get_param("not_a_scan"), "`x` must be a scan object.")
  expect_error(get_param(vp),"`x` must be a scan object.")
  expect_error(get_param(pvol),"`x` must be a scan object.")
  expect_error(get_param(scan))
  expect_error(get_param(scan, "not_a_param"), "Scan parameter not_a_param not found in `x`.")
})

test_that("get_param returns correct parameters", {
  expect_equal(get_param(scan, names(scan$params[1])), scan$params[[1]])
  expect_equal(get_param(scan, names(scan$params[2])), scan$params[[2]])
})

