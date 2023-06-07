data(example_scan)
example.plot <- plot(example_scan, param = "VRADH")

test_that("plot.scan() returns error on incorrect parameters", {
  # use plot.scan() to avoid defaulting to base plot()
  expect_error(plot.scan("not_a_scan"),
               regexp = 'inherits(x, "scan") is not TRUE',
               fixed = TRUE)
  expect_error(plot(example_scan, param = "not_a_param"),
               regexp = "parameter 'not_a_param' not found in scan",
               fixed = TRUE)
  expect_error(plot(example_scan, quantity = "DBZH"),
               regexp = "unknown function argument 'quantity`. Did you mean `param`?",
               fixed = TRUE)
})

test_that("plot.scan() defaults to DBZH on missing param", {
  expect_equal(plot(example_scan), plot(example_scan, param = "DBZH"))
})

test_that("plot.scan() returns ggplot object", {
  expect_s3_class(plot(example_scan), c("gg", "ggplot"))
})

test_that("plot.scan() uses the default zlim from color_scale", {
  expect_equal(range(example.plot$data$VRADH, na.rm = TRUE), get_zlim("VRADH", zlim))
})

test_that("plot.scan() takes zlim argument", {
  example.plot_zlim <- plot(example_scan, param = "VRADH", zlim = c(-5, 5))
  expect_equal(range(example.plot_zlim$data$VRADH, na.rm = TRUE), c(-5, 5))
})

test_that("plot.scan() uses param argument", {
  expect_true("VRADH" %in% names(example.plot$data))
})

test_that("plot.scan() returns as much data as in the scan", {
  expect_equal(length(example.plot$data$VRADH), length(example_scan$params$VRADH))
})
