ppi <- project_as_ppi(example_scan)
vp <- example_vp

# No tests for error on incorrect parameters:
# summary(), print(), dim() are generic and work for every input
# is.ppi() returns TRUE/FALSE and works for every input

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

test_that("dim.ppi() returns dimensions", {
  expect_vector(dim(ppi))
  expect_equal(dim(ppi), c(5, 200, 200)) # 5 param, 200 x pixels, 200 y pixels for default range_max
})

test_that("[.ppi subsets by param", {
  # parameters:  VRADH DBZH ZDR RHOHV PHIDP
  expect_equal(names(ppi[1]$data), c("VRADH"))
  expect_equal(names(ppi[2:4]$data), c("DBZH", "ZDR", "RHOHV"))
})
