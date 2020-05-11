test_that("Example objects are of the correct class", {
  expect_s3_class(example_scan, "scan")
  expect_s3_class(example_vp, "vp")
  expect_s3_class(example_vpts, "vpts")
})
