test_that("Integrate_profile() return vpi", {
  expect_s3_class(integrate_profile(example_vp), "vpi")
  expect_s3_class(integrate_profile(c(example_vp,example_vp)), "vpi")
  expect_s3_class(integrate_profile(example_vpts), "vpi")
})

test_that("integrate_profile() returns error on incorrect parameters", {
  expect_error(integrate_profile(example_vp, alt_min = 'notANumeric'))
  expect_error(integrate_profile(example_vp, alt_max = 'notANumeric'))
  expect_error(integrate_profile(example_vp, alpha = 'notANumeric'))
  expect_error(integrate_profile(example_vp, alt_min = 500, alt_max = 300))
})
