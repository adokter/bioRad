vp <- example_vp
vp_list <- c(example_vp, example_vp)
vp_list_mixed <- list(example_vp, "not_a_vp")
vpts <- example_vpts

test_that("sd_vvp_threshold() returns error on incorrect parameters", {
  expect_error(sd_vvp_threshold("not_a_vp"))
  expect_error(sd_vvp_threshold(vp_list_mixed), "`x` must be list of vp objects.")
})

test_that("sd_vvp_threshold()<- returns error on incorrect parameters", {
  expect_error(sd_vvp_threshold(vp) <- "not_a_double")
  expect_error(sd_vvp_threshold(vp) <- NULL)
  expect_error(sd_vvp_threshold(vp) <- c(2, 2))
  expect_error(sd_vvp_threshold("not_a_vp") <- 2)
  expect_error(sd_vvp_threshold(vp_list_mixed) <- 2, "`x` must be list of vp objects.")
})

test_that("sd_vvp_threshold() returns the correct sd_vvp_thresh", {
  expect_equal(sd_vvp_threshold(vp), vp$attributes$how$sd_vvp_thresh)
  expect_equal(
    sd_vvp_threshold(vp_list),
    c(vp$attributes$how$sd_vvp_thresh, vp$attributes$how$sd_vvp_thresh)
  )
  expect_equal(sd_vvp_threshold(vpts), vpts$attributes$how$sd_vvp_thresh)
})

test_that("sd_vvp_threshold()<- updates sd_vvp_thresh", {
  sd_vvp_threshold(vp) <- 5.5
  sd_vvp_threshold(vp_list) <- 5.5
  sd_vvp_threshold(vpts) <- 5.5

  expect_equal(vp$attributes$how$sd_vvp_thresh, 5.5)
  expect_equal(c(vp$attributes$how$sd_vvp_thresh, vp$attributes$how$sd_vvp_thresh), c(5.5, 5.5))
  expect_equal(vpts$attributes$how$sd_vvp_thresh, 5.5)
})

test_that("sd_vvp_threshold()<- updates density", {
  # Not tested for vp_list as that is a repetition of vp method

  # Set a sd_vvp_threshold
  sd_vvp_threshold(vp) <- 4
  sd_vvp_threshold(vpts) <- 4

  # If above sd_vvp_threshold: dens = eta/rcs
  expect_equal(
    vp$data$dens[vp$data$sd_vvp >= 4], vp$data$eta[vp$data$sd_vvp >= 4] / vp$attributes$how$rcs_bird
  )
  expect_equal(
    vpts$data$dens[vpts$data$sd_vvp >= 4], vpts$data$eta[vpts$data$sd_vvp >= 4] / vpts$attributes$how$rcs_bird
  )

  # If below sd_vvp_threshold and not NA: dens = 0
  expect_true(any(vp$data$dens[vp$data$sd_vvp < 4] == 0, na.rm = TRUE))
  expect_true(any(vpts$data$dens[vpts$data$sd_vvp < 4] == 0, na.rm = TRUE))
})

test_that("sd_vvp_threshold()<- sets rcs to 11 when NULL", {
  # Not tested for vp_list as that is a repetition of vp method

  vp$attributes$how$rcs_bird <- NULL
  vpts$attributes$how$rcs_bird <- NULL
  expect_warning(sd_vvp_threshold(vp) <- 4, "Radar cross section not set, defaulting to 11 cm^2.", fixed = TRUE)
  expect_warning(sd_vvp_threshold(vpts) <- 4, "Radar cross section not set, defaulting to 11 cm^2.", fixed = TRUE)
  expect_equal(rcs(vp), 11)
  expect_equal(rcs(vpts), 11)
})
