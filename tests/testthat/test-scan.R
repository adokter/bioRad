scan <- example_scan
vp <- example_vp

# No tests for error on incorrect parameters:
# summary(), print(), dim() are generic and work for every input
# is.scan() returns TRUE/FALSE and works for every input

test_that("summary.scan() prints metadata to the console", {
  # print.scan() is not tested as it is the same as and called from summary.scan()
  expect_output(summary(scan), "Polar scan (class scan)", fixed = TRUE)
  expect_output(summary(scan), "parameters:", fixed = TRUE)
  expect_output(summary(scan), "elevation angle:", fixed = TRUE)
  expect_output(summary(scan), "dims:", fixed = TRUE)

})

test_that("is.scan() returns TRUE/FALSE correctly", {
  expect_true(is.scan(scan))
  expect_false(is.scan("not_a_scan"))
  expect_false(is.scan(vp))
})

test_that("dim.scan() returns dimensions", {
  expect_vector(dim(scan))
  expect_equal(dim(scan), c(5, 480, 360)) # 5 param, 480 bins, 360 rays in example_scan
})
