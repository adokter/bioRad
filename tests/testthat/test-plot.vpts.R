example_vpts_reg <-
  suppressMessages(regularize_vpts(example_vpts))

test_that("plot.vpts() returns error on incorrect parameters", {
  # use plot.vpts() to avoid defaulting to base plot()
  expect_error(plot.vpts("not_a_vpts"),
               regexp = 'inherits(x, "vpts") is not TRUE',
               fixed = TRUE)
  expect_error(
    plot(example_vpts_reg, quantity = "not_a_quantity"),
    regexp = "`quantity` needs to be one of `u`, `v`, `w`, `ff`, `dd`, `sd_vvp`, `gap`, `dbz`, `eta`, `dens`, `DBZH`, `n`, `n_dbz`, `n_all` or `n_dbz_all`",
    fixed = TRUE)

  # Test error on "param" instead of "quantity"
  expect_error(
    plot(example_vpts_reg, param = "dens"),
    regexp = "unknown function argument 'param`. Did you mean `quantity`?",
    fixed = TRUE)
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

test_that("plot.vpts() returns error on bad palette", {
  expect_error(
    suppressMessages(plot(regularize_vpts(example_vpts), palette = 123)),
    regexp = "palette should be a character vector with hex color values",
    fixed = TRUE
  )
})

test_that("plot_wind_barbs() returns error on incorrect parameters", {
  expect_error(
    plot_wind_barbs(c(1, 2), c(4, 5, 8)),
    regexp = "X AND Y COORDINATES SHOULD HAVE SAME LENGTH!",
    fixed = TRUE
  )

  variable_error_message <-
    "ALL VARIABLES SHOULD HAVE SAME LENGTH AS COORDINATES, OR BE MISSING!!!"

  expect_error(
    plot_wind_barbs(c(1, 2), c(4, 5)),
    regexp = variable_error_message,
    fixed = TRUE
  )
  expect_error(
    plot_wind_barbs(c(1, 2), c(4, 5),
                    speed = 46,
                    direction = NA),
    regexp = variable_error_message,
    fixed = TRUE
  )
  expect_error(
    plot_wind_barbs(c(1, 2), c(4, 5),
                    speed = NA,
                    direction = NA,
                    fill = "red"),
    regexp = variable_error_message,
    fixed = TRUE
  )
  expect_error(
    plot_wind_barbs(c(1, 2), c(4, 5),
                    speed = NA,
                    direction = c(4,98,2,42),
                    fill = NA),
    regexp = variable_error_message,
    fixed = TRUE
  )
  expect_error(
    plot_wind_barbs(c(1, 2), c(4, 5),
                    speed = c(4,98,2,42),
                    direction = NA,
                    fill = NA),
    regexp = variable_error_message,
    fixed = TRUE
  )
  expect_error(
    plot_wind_barbs(c(1, 2), c(4, 5),
                    speed = NA,
                    direction = NA,
                    fill = c("slategray2","wheat3","tomato")),
    regexp = variable_error_message,
    fixed = TRUE
  )
})

test_that("plot.vpts() produces plots", {
  expect_s3_class(recordPlot(plot(example_vpts_reg)), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vpts_reg, quantity = "eta")), "recordedplot")
  expect_s3_class(recordPlot(plot(example_vpts_reg, quantity = "DBZH")), "recordedplot")
})
