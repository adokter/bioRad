vp <- example_vp
vp_list <- c(example_vp, example_vp)
vpts <- example_vpts

test_that("Test parameters", {
  expect_error(rcs("incorrect_data_type"))
  expect_error(rcs(vp) <- "incorrect_data_type")
  # expect_error(rcs(vp) <- -11) # Should not allow negative
})

test_that("Test output type", {
  expect_type(rcs(vp), "double")
  expect_type(rcs(vp_list), "double")
  expect_length(rcs(vp_list), 2)
  expect_type(rcs(vpts), "double")

  # Testing that object is of same class after rcs()<- assignment seemed
  # excessive
})

test_that("Test output value", {
  expect_equal(rcs(vp), 11)
  expect_equal(rcs(vp_list), c(11, 11))
  expect_equal(rcs(vpts), 11)

  # Assign rcs()<-
  rcs(vp) <- 5.1
  rcs(vp_list) <- 5.1
  rcs(vpts) <- 5.1

  expect_equal(rcs(vp), 5.1)
  expect_equal(rcs(vp_list), c(5.1, 5.1))
  expect_equal(rcs(vpts), 5.1)
})
