vp <- example_vp
vp_list <- c(example_vp, example_vp)
vp_list_mixed <- list(example_vp, "not_a_vp")
vpts <- example_vpts
vpi <- integrate_profile(example_vpts)

test_that("rcs() returns error on incorrect parameters", {
  expect_error(rcs("not_a_vp"))
  expect_error(rcs(vp_list_mixed), "`x` must be list of vp objects.")
})

test_that("rcs()<- returns error on incorrect parameters", {
  expect_error(rcs(vp) <- "not_a_double")
  expect_error(rcs(vp) <- NULL)
  expect_error(rcs(vp) <- c(2, 2))
  expect_error(rcs("not_a_vp") <- 5)
  expect_error(rcs(vp_list_mixed) <- 5, "`x` must be list of vp objects.")
  expect_error(rcs(vp) <- -11)
})

test_that("rcs() returns the correct rcs", {
  expect_equal(rcs(vp), vp$attributes$how$rcs)
  expect_equal(rcs(vp_list), c(vp$attributes$how$rcs, vp$attributes$how$rcs))
  expect_equal(rcs(vpts), vpts$attributes$how$rcs)
  expect_equal(rcs(vpi), attributes(vpi)$rcs)
})

test_that("rcs()<- updates rcs", {
  rcs(vp) <- 5.5
  rcs(vp_list) <- 5.5
  rcs(vpts) <- 5.5
  rcs(vpi) <- 5.5

  expect_equal(vp$attributes$how$rcs_bird, 5.5)
  expect_equal(c(vp$attributes$how$rcs_bird, vp$attributes$how$rcs_bird), c(5.5, 5.5))
  expect_equal(vpts$attributes$how$rcs_bird, 5.5)
  expect_equal(attributes(vpi)$rcs, 5.5)
})

test_that("rcs()<- updates density", {
  # Not tested for vp_list as that is a repetition of vp method

  # rcs()<- should set none-NA densities to 0 if below sd_vvp_threshold.
  # In the example_vp, all densities are NA below its sd_vvp_threshold of 2, so
  # we set a higher sd_vvp_threshold to have some none-NA values:
  vp$attributes$how$sd_vvp_thresh <- 4
  vpts$attributes$how$sd_vvp_thresh <- 4

  # Set rcs
  rcs(vp) <- 6
  rcs(vpts) <- 6

  # If above sd_vvp_threshold: dens = eta/rcs
  expect_equal(
    vp$data$dens[vp$data$sd_vvp >= 4], vp$data$eta[vp$data$sd_vvp >= 4] / 6
  )
  expect_equal(
    vpts$data$dens[vpts$data$sd_vvp >= 4], vpts$data$eta[vpts$data$sd_vvp >= 4] / 6
  )
  # If below sd_vvp_threshold and not NA: dens = 0
  expect_true(all(vp$data$dens[vp$data$sd_vvp < 4] == 0, na.rm = TRUE))
  expect_true(all(vpts$data$dens[vpts$data$sd_vvp < 4] == 0, na.rm = TRUE))
})

test_that("rcs()<- sets sd_vvp_threshold to 2 when NULL", {
  # Not tested for vp_list as that is a repetition of vp method

  vp$attributes$how$sd_vvp_thresh <- NULL
  vpts$attributes$how$sd_vvp_thresh <- NULL
  expect_warning(rcs(vp) <- 6, "Threshold for sd_vvp not set, defaulting to 2 m/s.")
  expect_warning(rcs(vpts) <- 6, "Threshold for sd_vvp not set, defaulting to 2 m/s.")
  expect_equal(sd_vvp_threshold(vp), 2)
  expect_equal(sd_vvp_threshold(vpts), 2)
})

test_that("rcs()<- updates mtr and vid in vpi", {
  rcs(vpi) <- 5.5
  expect_equal(vpi$mtr, vpi$rtr / 5.5)
  expect_equal(vpi$vid, vpi$vir / 5.5)
})
