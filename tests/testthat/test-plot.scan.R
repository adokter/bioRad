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

test_that("plot.scan() creates expected graph", {

  # First time run it creates a snapshot, stored under
  # testthat/_snap/function_name. Then each time creates new snapshot and checks
  # that it looks identical to previously saved snapshot. If it fails, the new
  # snapshot is stored with ".new" appended to name.

  vdiffr::expect_doppelganger(
    "example_plot_DBZH",
    plot.scan(example_scan, param = "DBZH")
  )
  vdiffr::expect_doppelganger(
    "example_plot_VRADH",
    plot.scan(example_scan, param = "VRADH")
  )
  vdiffr::expect_doppelganger(
    "example_plot_RHOHV",
    plot.scan(example_scan, param = "RHOHV")
  )
  vdiffr::expect_doppelganger(
    "example_plot_PHIDP",
    plot.scan(example_scan, param = "PHIDP")
  )
  vdiffr::expect_doppelganger(
    "example_plot_ZDR",
    plot.scan(example_scan, param = "ZDR")
  )
})
