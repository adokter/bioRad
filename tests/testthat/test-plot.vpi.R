example_vpi <- integrate_profile(example_vpts)

test_that("plot.vpi() returns error on incorrect parameters", {
  expect_error(plot.vpi("not_a_vpi"))
  expect_error(plot.vpi(example_vpi, quantity = "not_a_quantity"))

  # Test error on "param" instead of "quantity"
  expect_error(plot.vpi(example_vpi, param = "ff"))

  # Test warning for deprecated arguments
  expect_warning(plot.vpi(example_vpi, line.col = "red"))
  expect_warning(plot.vpi(example_vpi, line.lwd = 1))
  })

test_that("plot.vpi() creates expected graph", {
  # On first run: creates a snapshot, stored under testthat/_snap/function_name.
  # On consecutive runs: creates new snapshot and checks that it looks identical
  # to previously saved snapshot. If it fails, a warning is given and the new
  # snapshot is stored with ".new" appended to name for manual inspection.

  vdiffr::expect_doppelganger(
    "example_plot_mtr",
     plot.vpi(example_vpi, quantity = "mtr")
  )
  vdiffr::expect_doppelganger(
    "example_plot_rtr",
    plot.vpi(example_vpi, quantity = "rtr")
  )
  vdiffr::expect_doppelganger(
    "example_plot_vid",
     plot.vpi(example_vpi, quantity = "vid")
  )
  vdiffr::expect_doppelganger(
    "example_plot_vir",
    plot.vpi(example_vpi,quantity = "vir")
  )
  vdiffr::expect_doppelganger(
    "example_plot_mt",
    plot.vpi(example_vpi, quantity = "mt")
  )
  vdiffr::expect_doppelganger(
    "example_plot_rt",
    plot.vpi(example_vpi, quantity = "rt")
  )
  vdiffr::expect_doppelganger(
    "example_plot_ff",
    plot.vpi(example_vpi, quantity = "ff")
  )
  vdiffr::expect_doppelganger(
    "example_plot_dd",
    plot.vpi(example_vpi, quantity = "dd")
  )
  vdiffr::expect_doppelganger(
    "example_plot_u",
    plot.vpi(example_vpi, quantity = "u")
  )
  vdiffr::expect_doppelganger(
    "example_plot_v",
    plot.vpi(example_vpi, quantity = "v")
  )
  vdiffr::expect_doppelganger(
    "example_plot_height",
    plot.vpi(example_vpi, quantity = "height")
  )
})
