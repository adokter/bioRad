ppi <- project_as_ppi(example_scan)
vp <- example_vp

# No tests for error on incorrect parameters:
# summary(), print(), dim() are generic and work for every input
# is.ppi() returns TRUE/FALSE and works for every input

test_that("[.ppi returns error on incorrect parameters", {
  expect_error(ppi["not_numeric"])
})

test_that("summary.ppi() prints metadata to the console", {
  # print.ppi() is not tested as it is the same as and called from summary.ppi()
  expect_output(summary(ppi), "Plan position indicator (class ppi)", fixed = TRUE)
  expect_output(summary(ppi), "parameters:", fixed = TRUE)
  expect_output(summary(ppi), "dims:", fixed = TRUE)
})

test_that("is.ppi() returns TRUE/FALSE correctly", {
  expect_true(is.ppi(ppi))
  expect_false(is.ppi("not_a_ppi"))
  expect_false(is.ppi(vp))
})

test_that("dim.ppi() returns number of params, x, y", {
  expect_vector(dim(ppi))
  expect_equal(dim(ppi)[1], 5) # 5 param
  expect_equal(dim(ppi)[2:3], c(200, 200), tolerance = 1) # 200 x, 200 y with tolerance
})

test_that("[.ppi subsets by param", {
  # parameters:  VRADH DBZH ZDR RHOHV PHIDP
  expect_equal(names(ppi[1]$data), c("VRADH"))
  expect_equal(names(ppi[2:4]$data), c("DBZH", "ZDR", "RHOHV"))
  expect_equal(names(ppi[-2:-4]$data), c("VRADH", "PHIDP")) # All except 2 to 4
})
