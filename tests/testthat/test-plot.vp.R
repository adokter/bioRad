test_that("plot.vp() returns error on incorrect parameters", {
  expect_error(plot.vp("not_a_vp"))
  expect_error(plot.vp(example_vp, quantity = "not_a_quantity"))

  # Test error on "param" instead of "quantity"
  expect_error(plot.vp(example_vp, param = "ff"))

  # Test warning for deprecated arguments
  expect_warning(plot.vp(example_vp, line.col = "red"))
  expect_warning(plot.vp(example_vp, line.lwd = 1))
})

test_that("plot.vp() creates expected graph", {
  # On first run: creates a snapshot, stored under testthat/_snap/function_name.
  # On consecutive runs: creates new snapshot and checks that it looks identical
  # to previously saved snapshot. If it fails, a warning is given and the new
  # snapshot is stored with ".new" appended to name for manual inspection.

  vdiffr::expect_doppelganger(
    "example_plot_ff",
    plot.vp(example_vp, quantity = "ff")
  )
  vdiffr::expect_doppelganger(
    "example_plot_dd",
    plot.vp(example_vp, quantity = "dd")
  )
  vdiffr::expect_doppelganger(
    "example_plot_dens",
    plot.vp(example_vp, quantity = "dens")
  )
  vdiffr::expect_doppelganger(
    "example_plot_eta",
    plot.vp(example_vp, quantity = "eta")
  )
  vdiffr::expect_doppelganger(
    "example_plot_dbz",
    plot.vp(example_vp, quantity = "dbz")
  )
  vdiffr::expect_doppelganger(
    "example_plot_DBZH",
    plot.vp(example_vp, quantity = "DBZH")
  )
})
