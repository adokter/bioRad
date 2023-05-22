data(example_scan)
example.plot <- plot(example_scan, param = "VRADH", zlim = c(-5, 5))

test_that("plot() returns error on incorrect parameters", {
  expect_error(plot("not_a_scan"))
  expect_error(plot(example_scan, param = "not_a_param"))
  expect_error(plot(example_scan, quantity = "DBZH"))
})

test_that("plot() defaults to DBZH on missing param", {
  expect_equal(plot(example_scan), plot(example_scan, param = "DBZH"))
})

test_that("plot() returns ggplot object", {
  expect_true(all(class(plot(example_scan)) == c("gg", "ggplot")))
})

test_that("plot() takes zlim arguments", {
  expect_true(all(example.plot$data$VRADH >= -5, na.rm = TRUE))
  expect_true(all(example.plot$data$VRADH <= 5, na.rm = TRUE))
})

test_that("plot() uses param argument", {
  expect_true("VRADH" %in% names(example.plot$data))
})

test_that("plot() returns as much data as in the scan", {
  expect_equal(length(example.plot$data$VRADH), length(example_scan$params$VRADH))
})

 test_that("plot() creates expected graph", {
   # On first run: creates a snapshot, stored under testthat/_snap/function_name.
   # On consecutive runs: creates new snapshot and checks that it looks identical
   # to previously saved snapshot. If it fails, a warning is given and the new
   # snapshot is stored with ".new" appended to name for manual inspection.

   vdiffr::expect_doppelganger(
     "example_plot_DBZH",
     plot(example_scan, param = "DBZH")
   )
   vdiffr::expect_doppelganger(
     "example_plot_VRADH",
     plot(example_scan, param = "VRADH")
   )
   vdiffr::expect_doppelganger(
     "example_plot_RHOHV",
     plot(example_scan, param = "RHOHV")
   )
   vdiffr::expect_doppelganger(
     "example_plot_PHIDP",
     plot(example_scan, param = "PHIDP")
   )
   vdiffr::expect_doppelganger(
     "example_plot_ZDR",
     plot(example_scan, param = "ZDR")
   )
 })
