example_vpts_reg <- regularize_vpts(example_vpts)

test_that("plot() returns error on incorrect parameters", {
  expect_error(plot("not_a_vpts"))
  expect_error(plot(example_vpts_reg, quantity = "not_a_quantity"))

  # Test error on "param" instead of "quantity"
  expect_error(plot(example_vpts_reg, param = "dens"))

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

  # Test warning for deprecated arguments
  expect_warning(plot(example_vpts_reg, quantity = "dens", barbs.h = 1))
  expect_warning(plot(example_vpts_reg, quantity = "dens", barbs.t = 1))
  expect_warning(plot(example_vpts_reg, quantity = "dens", barbs.dens = 1))
  expect_warning(plot(example_vpts_reg, quantity = "dens", legend.ticks = 1))
})

test_that("plot() creates expected graph", {
   # On first run: creates a snapshot, stored under testthat/_snap/function_name.
   # On consecutive runs: creates new snapshot and checks that it looks identical
   # to previously saved snapshot. If it fails, a warning is given and the new
   # snapshot is stored with ".new" appended to name for manual inspection.

   vdiffr::expect_doppelganger(
     "example_plot_dens",
     plot(example_vpts_reg, quantity = "dens")
   )
   vdiffr::expect_doppelganger(
     "example_plot_eta",
     plot(example_vpts_reg, quantity = "eta")
   )
   vdiffr::expect_doppelganger(
     "example_plot_dbz",
     plot(example_vpts_reg, quantity = "dbz")
   )
   vdiffr::expect_doppelganger(
     "example_plot_DBZH",
     plot(example_vpts_reg,quantity = "DBZH")
   )
   vdiffr::expect_doppelganger(
     "example_plot_ff",
     plot(example_vpts_reg,quantity = "ff")
   )
   vdiffr::expect_doppelganger(
     "example_plot_u",
     plot(example_vpts_reg,quantity = "u")
   )
   vdiffr::expect_doppelganger(
     "example_plot_v",
     plot(example_vpts_reg,quantity = "v")
   )

 })
