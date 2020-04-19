vp <- example_vp
vp_list <- c(example_vp, example_vp)
vp_list_mixed <- list(example_vp, "not_a_vp")
vpts <- example_vpts
vpi <- integrate_profile(example_vpts)

test_that("returns error on incorrect parameters", {
  expect_error(rcs("not_a_vp"))
  expect_error(rcs(vp_list_mixed), "Input must be list of vp objects.")

  expect_error(rcs(vp) <- "not_a_double")
  expect_error(rcs(vp) <- NULL)
  expect_error(rcs(vp) <- c(2, 2))
  expect_error(rcs("not_a_vp") <- 5)
  expect_error(rcs(vp_list_mixed) <- 5, "Input must be list of vp objects.")
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

  # Precondition: at least some of the dens > 0 below sd_vvp_threshold
  # This is not the case for sd_vvp_threshold = 2, which is why it is set to 3
  vp$attributes$how$sd_vvp_thresh <- 3
  vpts$attributes$how$sd_vvp_thresh <- 3
  expect_true(any(vp$data$dens[vp$data$sd_vvp < 3] > 0, na.rm = TRUE))
  expect_true(any(vpts$data$dens[vpts$data$sd_vvp < 3] > 0, na.rm = TRUE))

  rcs(vp) <- 6
  rcs(vpts) <- 6

  # dens = eta/rcs when above sd_vvp_threshold
  expect_equal(
    vp$data$dens[vp$data$sd_vvp >= 3], vp$data$eta[vp$data$sd_vvp >= 3] / 6
  )
  expect_equal(
    vpts$data$dens[vpts$data$sd_vvp >= 3], vpts$data$eta[vpts$data$sd_vvp >= 3] / 6
  )
  # dens = 0 when below sd_vvp_threshold and not NA
  expect_true(all(vp$data$dens[vp$data$sd_vvp < 3] == 0, na.rm = TRUE))
  expect_true(all(vpts$data$dens[vpts$data$sd_vvp < 3] == 0, na.rm = TRUE))
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
