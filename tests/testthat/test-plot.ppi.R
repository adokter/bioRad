
test_that("plot.ppi() returns error on incorrect parameters", {
  expect_error(plot.ppi("not_a_ppi"))
  expect_error(plot.ppi("not_a_object"))
})
