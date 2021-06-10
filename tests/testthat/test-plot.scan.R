data(example_scan)
example.plot <- plot(example_scan, param = "VRADH", zlim = c(-5, 5))

test_that("plot.scan() returns error on incorrect parameters", {
  expect_error(plot.scan("not_a_scan"))
  expect_error(plot.scan(example_scan, param = "not_a_param"))
  expect_error(plot.scan(example_scan, quantity = "DBZH"))
})

test_that("if param missing default to DBZH", {
  expect_equal(plot.scan(example_scan), plot.scan(example_scan, param = "DBZH"))
})

test_that("plot.scan() returns ggplot object", {
  expect_true(all(class(plot.scan(example_scan)) == c("gg", "ggplot")))
})

test_that("plot.scan() takes zlim arguments", {
  expect_true(all(example.plot$data$VRADH >= -5, na.rm = TRUE))
  expect_true(all(example.plot$data$VRADH <= 5, na.rm = TRUE))
})

test_that("plot.scan() uses param argument", {
  expect_true("VRADH" %in% names(example.plot$data))
})

test_that("length does not differ", {
  expect_equal(length(example.plot$data$VRADH), length(example_scan$params$VRADH))
})

