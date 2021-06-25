data(example_scan)
ppi <- project_as_ppi(example_scan)
example.plot <-plot(ppi, param = "VRADH", zlim = c(-5, 5))

test_that("plot.ppi() returns error on incorrect parameters", {
  expect_error(plot.ppi("not_a_ppi"))
  expect_error(plot.ppi(ppi, param = "not_a_param"))
  expect_error(plot.ppi(ppi, quantity = "DBZH"))
})

test_that("if param missing default to DBZH", {
  expect_equal(plot.ppi(ppi), plot.ppi(ppi, param = "DBZH"))
})

test_that("plot.ppi() returns ggplot object", {
  expect_true(all(class(plot.ppi(ppi)) == c("gg", "ggplot")))
})

test_that("plot.ppi() takes zlim arguments", {
  expect_true(all(example.plot$data$VRADH >= -5, na.rm = TRUE))
  expect_true(all(example.plot$data$VRADH <= 5, na.rm = TRUE))
})

test_that("plot.ppi() uses param argument", {
  expect_true("VRADH" %in% names(example.plot$data))
})

test_that("length does not differ", {
  expect_equal(length(example.plot$data$VRADH), length(ppi$data$VRADH))
})
