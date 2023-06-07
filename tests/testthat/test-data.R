test_that("Example objects are of the correct class", {
  expect_s3_class(example_scan, "scan")
  expect_s3_class(example_vp, "vp")
  expect_s3_class(example_vpts, "vpts")
})

test_that("Example scan includes all five parameters", {
  expect_no_error(get_param(example_scan, 'DBZH'))
  expect_no_error(get_param(example_scan, 'VRADH'))
  expect_no_error(get_param(example_scan, 'RHOHV'))
  expect_no_error(get_param(example_scan, 'ZDR'))
  expect_no_error(get_param(example_scan, 'PHIDP'))
})

test_that("Example scan can be plotted correctly", {
  expect_no_error(plot(example_scan, param = "DBZH"))
  expect_no_error(plot(example_scan, param = "VRADH"))
  expect_no_error(plot(example_scan, param = "RHOHV"))
  expect_no_error(plot(example_scan, param = "ZDR"))
  expect_no_error(plot(example_scan, param = "PHIDP"))
})

test_that("Example vp can be plotted correctly", {
  expect_no_error(plot(example_vp, quantity = "DBZH"))
  expect_no_error(plot(example_vp, quantity = "dbz"))
  expect_no_error(plot(example_vp, quantity = "eta"))
  expect_no_error(plot(example_vp, quantity = "dens"))
})

test_that("Example vpts can be plotted correctly", {
  expect_no_error(
    plot(regularize_vpts(example_vpts, verbose = FALSE), quantity = "DBZH"))
  expect_no_error(
    plot(regularize_vpts(example_vpts, verbose = FALSE), quantity = "dbz"))
  expect_no_error(
    plot(regularize_vpts(example_vpts, verbose = FALSE), quantity = "eta"))
  expect_no_error(
    plot(regularize_vpts(example_vpts, verbose = FALSE), quantity = "dens"))
})
