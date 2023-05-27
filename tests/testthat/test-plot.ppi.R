data(example_scan)
ppi <- project_as_ppi(example_scan)
example.plot <-plot(ppi, param = "VRADH", zlim = c(-5, 5))

test_that("plot.ppi() returns error on incorrect parameters", {
  # use plot.ppi() to avoid defaulting to base plot()
  expect_error(plot.ppi("not_a_ppi"))
  expect_error(plot(ppi, param = "not_a_param"))
  expect_error(plot(ppi, quantity = "DBZH"))
})

test_that("plot.ppi() defaults to DBZH if param missing", {
  expect_equal(plot(ppi), plot(ppi, param = "DBZH"))
})

test_that("plot.ppi() returns ggplot object", {
  expect_s3_class(plot(ppi), c("gg", "ggplot"))
})

test_that("plot.ppi() takes zlim arguments", {
  expect_true(all(example.plot$data$VRADH >= -5, na.rm = TRUE))
  expect_true(all(example.plot$data$VRADH <= 5, na.rm = TRUE))
})

test_that("plot.ppi() uses param argument", {
  expect_true("VRADH" %in% names(example.plot$data))
})

test_that("plot.ppi()length does not differ", {
  expect_equal(length(example.plot$data$VRADH), length(ppi$data$VRADH))
})

