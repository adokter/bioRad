test_that("plot.vp() returns error on incorrect parameters", {
  # use plot.vp() to avoid defaulting to base plot()
  expect_error(plot.vp("not_a_vp"),
    regexp = 'inherits(x, "vp") is not TRUE',
    fixed = TRUE
  )
  expect_error(plot(example_vp, quantity = "not_a_quantity"),
    regexp = "unknown quantity 'not_a_quantity'",
    fixed = TRUE
  )

  # Test error on "param" instead of "quantity"
  expect_error(plot(example_vp, param = "ff"),
    regexp = "unknown function argument 'param`. Did you mean `quantity`?",
    fixed = TRUE
  )
})

test_that("plot.vp() warns for deprecated arguments", {
  expect_warning(
    plot(example_vp, line.col = "red"),
    regexp = "argument line.col is deprecated; please use line_col instead.",
    fixed = TRUE
  )
  expect_warning(
    plot(example_vp, line.lwd = 1),
    regexp = "argument line.lwd is deprecated; please use line_lwd instead.",
    fixed = TRUE
  )
})


test_that("plot.vp() produces plots", {
  expect_s3_class(recordPlot(plot(example_vp)), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vp, quantity = "ff")), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vp, quantity = "DBZH")), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vp, quantity = "dens")), "recordedplot")
})
