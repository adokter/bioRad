example_vpi <- integrate_profile(example_vpts)

test_that("plot.vpi() returns error on incorrect parameters", {
  # use plot.vpts() to avoid defaulting to base plot()
  expect_error(plot.vpi("not_a_vpi"))
  expect_error(plot(example_vpi, quantity = "not_a_quantity"))

  # Test error on "param" instead of "quantity"
  expect_error(plot(example_vpi, param = "ff"))
})

test_that("plot.vpi() warns for deprecated arguments", {
  expect_warning(plot(example_vpi, line.col = "red"))
  expect_warning(plot(example_vpi, line.lwd = 1))
})

test_that("plot.vpi() produces plots", {
  expect_s3_class(recordPlot(plot(example_vpi)), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vpi, quantity = "vir")), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vpi, quantity = "mtr")), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vpi, quantity = "dd")), "recordedplot")
})
