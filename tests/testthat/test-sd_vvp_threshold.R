vp <- example_vp
vp_list <- c(example_vp, example_vp)
suppressWarnings(
  vp_list_mixed <- c(example_vp, "not_a_vp") # Warns: Non-vp objects found!
)
vpts <- example_vpts

test_that("returns error on incorrect parameters", {
  expect_error(sd_vvp_threshold("not_a_vp"))
  expect_error(sd_vvp_threshold(vp_list_mixed),
               "Input must be list of vp objects.")

  expect_error(sd_vvp_threshold(vp) <- "not_a_double")
  expect_error(sd_vvp_threshold(vp) <- NULL)
  expect_error(sd_vvp_threshold(vp) <- c(2,2))
  expect_error(sd_vvp_threshold("not_a_vp") <- 2)
  expect_error(sd_vvp_threshold(vp_list_mixed) <- 2,
               "Input must be list of vp objects.")

})

test_that("returns the correct sd threshold", {
expect_equal(sd_vvp_threshold(vp), vp$attributes$how$sd_vvp_thresh)
expect_equal(sd_vvp_threshold(vp_list), c(vp$attributes$how$sd_vvp_thresh,
                                          vp$attributes$how$sd_vvp_thresh))
expect_equal(sd_vvp_threshold(vpts), vpts$attributes$how$sd_vvp_thresh)

})

test_that("sd_vvp_threshold assignment updates sd_vvp_threshold", {
  sd_vvp_threshold(vp) <- 5.5
  sd_vvp_threshold(vp_list) <- 5.5
  sd_vvp_threshold(vpts) <- 5.5

  expect_equal(vp$attributes$how$sd_vvp_thresh, 5.5)
  expect_equal(c(vp$attributes$how$sd_vvp_thresh,
                 vp$attributes$how$sd_vvp_thresh),
               c(5.5, 5.5))
  expect_equal(vpts$attributes$how$sd_vvp_thresh, 5.5)
})

test_that("sd_vvp_threshold assignments updates density", {
  sd_vvp_threshold(vp) <- 3
  sd_vvp_threshold(vp_list) <- 3
  sd_vvp_threshold(vpts) <- 3

  expect_true(any(vp$data$dens[vp$data$sd_vvp < 3] == 0, na.rm = TRUE))
  expect_true(any(vpts$data$dens[vpts$data$sd_vvp < 3] == 0, na.rm = TRUE))
})
