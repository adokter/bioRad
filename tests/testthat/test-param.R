param <- get_param(example_scan, "DBZH")
vp <- example_vp

# No tests for error on incorrect parameters:
# summary(), print() are generic and work for every input
# is.param() returns TRUE/FALSE and works for every input

test_that("summary.param() prints metadata to the console", {
  # print.param() is not tested as it is the same as and called from summary.param()
  expect_output(summary(param), "Polar scan parameter (class param)", fixed = TRUE)
  expect_output(summary(param), "quantity:", fixed = TRUE)
  expect_output(summary(param), "dims:", fixed = TRUE)
})

test_that("is.param() returns TRUE/FALSE correctly", {
  expect_true(is.param(param))
  expect_false(is.param("not_a_param"))
  expect_false(is.param(vp))
})
