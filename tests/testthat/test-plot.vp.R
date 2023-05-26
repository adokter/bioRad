test_that("plot.vp() returns error on incorrect parameters", {
  # use plot.vp() to avoid defaulting to base plot()
  expect_error(plot.vp("not_a_vp"))
  expect_error(plot(example_vp, quantity = "not_a_quantity"))

  # Test error on "param" instead of "quantity"
  expect_error(plot(example_vp, param = "ff"))
})

test_that("plot.vp() warns for deprecated arguments", {
  expect_warning(plot(example_vp, line.col = "red"))
  expect_warning(plot(example_vp, line.lwd = 1))
})


test_that("plot.vp() produces plots", {
  expect_s3_class(recordPlot(plot(example_vp)), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vp, quantity = "ff")), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vp, quantity = "DBZH")), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vp, quantity = "dens")), "recordedplot")
})
