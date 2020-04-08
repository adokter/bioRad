vp <- example_vp
vp_list <- c(example_vp, example_vp)
suppressWarnings(
  vp_list_mixed <- c(example_vp, "not_a_vp") # Warns: Non-vp objects found!
)
vpts <- example_vpts
vpi <- integrate_profile(example_vpts)

test_that("returns errors on incorrect parameters", {
  expect_error(rcs("not_a_vp"))
  expect_error(rcs(vp_list_mixed), "Input must be list of vp objects.")

  expect_error(rcs(vp) <- "not_a_double")
  expect_error(rcs(vp) <- NULL)
  expect_error(rcs("not_a_vp") <- 5)
  expect_error(rcs(vp_list_mixed) <- 5, "Input must be list of vp objects.")
  # expect_error(rcs(vp) <- -11) # Should not allow negative
})

test_that("returns double", {
  expect_type(rcs(vp), "double")
  expect_type(rcs(vp_list), "double")
  expect_length(rcs(vp_list), 2) # vector of 2
  expect_type(rcs(vpts), "double")
  expect_type(rcs(vpi), "double")
})

test_that("returns 11 by default", {
  expect_equal(rcs(vp), 11)
  expect_equal(rcs(vp_list), c(11, 11))
  expect_equal(rcs(vpts), 11)
  expect_equal(rcs(vpi), 11)

  # Assign rcs()<-
  rcs(vp) <- 5.1
  rcs(vp_list) <- 5.1
  rcs(vpts) <- 5.1
  rcs(vpi) <- 5.1

  expect_equal(rcs(vp), 5.1)
  expect_equal(rcs(vp_list), c(5.1, 5.1))
  expect_equal(rcs(vpts), 5.1)
  expect_equal(rcs(vpi), 5.1)
})
