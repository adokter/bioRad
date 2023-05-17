pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)
scan <- example_scan

test_that("get_param() returns error on incorrect parameters", {
  expect_error(get_param("not_a_scan", "DBZH"), "`x` must be a scan object.", fixed = TRUE)
  expect_error(get_param(pvol, "DBZH"),"`x` must be a scan object.", fixed = TRUE)
  expect_error(get_param(scan)) # Parameter "param" missing
  expect_error(get_param(scan, "not_a_param"), "Can't find parameter `not_a_param` in `x`", fixed = TRUE)
})

test_that("get_param() returns correct parameter", {
  # Parameters: VRADH DBZH ZDR RHOHV PHIDP
  expect_equal(get_param(scan, "DBZH"), scan$params[["DBZH"]])
  expect_equal(get_param(scan, "ZDR"), scan$params[["ZDR"]])
  expect_equal(get_param(scan, "RHOHV"), scan$params[["RHOHV"]])
  expect_equal(get_param(scan, "VRADH"), scan$params[["VRADH"]])
  expect_equal(get_param(scan, "PHIDP"), scan$params[["PHIDP"]])

  expect_s3_class(get_param(scan, "RHOHV"), "param")
})
