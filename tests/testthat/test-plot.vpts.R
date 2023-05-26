example_vpts_reg <- regularize_vpts(example_vpts)

test_that("plot.vpts() returns error on incorrect parameters", {
  # use plot.vpts() to avoid defaulting to base plot()
  expect_error(plot.vpts("not_a_vpts"))
  expect_error(plot(example_vpts_reg, quantity = "not_a_quantity"))

  # Test error on "param" instead of "quantity"
  expect_error(plot(example_vpts_reg, param = "dens"))
})

test_that("plot.vpts() warns for certain datatypes", {
  # Test warning for not regularized vpts
  expect_warning(
    plot(example_vpts, quantity = "dens"),
    "Irregular time-series"
  )

  # Test warning for duplicated time steps
  example_vpts_dup <- example_vpts_reg
  example_vpts_dup$timesteps <- c(example_vpts_dup$timesteps, 0)
  expect_warning(
    plot(example_vpts_dup, quantity = "dens"),
    "duplicate datetime values"
  )
})

test_that("plot.vpts() warns for deprecated arguments", {
  expect_warning(plot(example_vpts_reg, quantity = "dens", barbs.h = 1))
  expect_warning(plot(example_vpts_reg, quantity = "dens", barbs.t = 1))
  expect_warning(plot(example_vpts_reg, quantity = "dens", barbs.dens = 1))
  expect_warning(plot(example_vpts_reg, quantity = "dens", legend.ticks = 1))
})

test_that("plot.vpts() produces a plot", {
  expect_s3_class(recordPlot(plot(example_vpts_reg)), "recordedplot")

})
