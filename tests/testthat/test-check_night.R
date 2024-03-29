test_that("check_night() returns error on incorrect parameters", {
  expect_error(check_night(x = list(example_vp,example_vpts)),
               regexp = "`x` must be list of `vp` objects.",
               fixed = TRUE)
})


test_that("check_night identifies night with numeric values", {
  expect_false(check_night(as.POSIXct("2016-1-1 12:00", tz = "UTC"), 5, 53))
})

test_that("check_night identifies day/night for example data", {
  expect_type(check_night(example_vp), "logical")
  expect_true(check_night(example_vp))

  expect_type(check_night(example_vpts), "logical")
  expect_true(check_night(example_vpts)[1])
  expect_false(check_night(example_vpts)[145])
  check_night(integrate_profile(example_vp))
})
