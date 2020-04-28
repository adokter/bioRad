pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)
scan <- pvol$scans[[1]]

test_that("get_param() returns error on incorrect parameters", {
  expect_error(get_param("not_a_scan", "DBZH"), "`x` must be a scan object.", fixed = TRUE)
  expect_error(get_param(pvol, "DBZH"),"`x` must be a scan object.", fixed = TRUE)
  expect_error(get_param(scan)) # Parameter "param" missing
  expect_error(get_param(scan, "not_a_param"), "Can't find parameter `not_a_param` in `x`", fixed = TRUE)
})

test_that("get_param() returns correct parameters", {
  expect_equal(get_param(scan, names(scan$params[1])), scan$params[[1]])
  expect_equal(get_param(scan, names(scan$params[2])), scan$params[[2]])
})
