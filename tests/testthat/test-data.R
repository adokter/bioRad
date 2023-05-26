test_that("Example objects are of the correct class", {
  expect_s3_class(example_scan, "scan")
  expect_s3_class(example_vp, "vp")
  expect_s3_class(example_vpts, "vpts")
})

test_that("Example objects are of correct class (2nd test)", {
  expect_true(is.scan(example_scan))
  expect_true(is.vp(example_vp))
  expect_true(is.vpts(example_vpts))
})

test_that("Example scan includes all five parameters", {
  expect_true(is.param(get_param(example_scan, 'DBZH')))
  expect_true(is.param(get_param(example_scan, 'VRADH')))
  expect_true(is.param(get_param(example_scan, 'RHOHV')))
  expect_true(is.param(get_param(example_scan, 'ZDR')))
  expect_true(is.param(get_param(example_scan, 'PHIDP')))
})

test_that("Plotting example scan produces no error", {
  expect_no_error(plot(example_scan, param = "DBZH"))
  expect_no_error(plot(example_scan, param = "VRADH"))
  expect_no_error(plot(example_scan, param = "RHOHV"))
  expect_no_error(plot(example_scan, param = "ZDR"))
  expect_no_error(plot(example_scan, param = "PHIDP"))
})

test_that("Plotting example vp produces no error", {
  expect_no_error(plot(example_vp, quantity  = "DBZH"))
  expect_no_error(plot(example_vp, quantity  = "dbz"))
  expect_no_error(plot(example_vp, quantity  = "eta"))
  expect_no_error(plot(example_vp, quantity  = "dens"))
})

test_that("Plotting example vpts produces no error", {
  expect_no_error(plot(regularize_vpts(example_vpts), quantity  = "DBZH"))
  expect_no_error(plot(regularize_vpts(example_vpts), quantity  = "dbz"))
  expect_no_error(plot(regularize_vpts(example_vpts), quantity  = "eta"))
  expect_no_error(plot(regularize_vpts(example_vpts), quantity  = "dens"))
})



