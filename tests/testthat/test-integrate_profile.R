test_that("Integrate_profile() return vpi", {
  expect_s3_class(integrate_profile(example_vp), "vpi")
  expect_s3_class(integrate_profile(c(example_vp,example_vp)), "vpi")
  expect_s3_class(integrate_profile(example_vpts), "vpi")
})

test_that("integrate_profile() returns error on incorrect parameters", {
  expect_error(integrate_profile(example_vp, alt_min = 'notANumeric'))
  expect_error(integrate_profile(example_vp, alt_max = 'notANumeric'))
  expect_error(integrate_profile(example_vp, alpha = 'notANumeric'))
  expect_error(integrate_profile(example_vpts, interval_max = 'notANumeric'))
  expect_error(integrate_profile(example_vpts, interval_replace = 'notANumeric'))
  expect_error(integrate_profile(example_vp, alt_min = 500, alt_max = 300))
  expect_error(integrate_profile(example_vp, height_quantile = "a"))
  expect_error(integrate_profile(example_vp, height_quantile = 2))
test_that("integrate_profile() returns error on non vp object in list input",{
  expect_error(integrate_profile(x = list(example_vp, example_vpts)),
               regexp = "requires list of vp objects as input",
               fixed = TRUE)
})

test_that("integrate_profile() checks value of interval_max", {
  expect_warning(integrate_profile(example_vpts, interval_max = 1))
  expect_warning(integrate_profile(example_vpts, interval_max = 1, interval_replace=10))
})

test_that("rotating transect by 180 degrees results in sign flip", {
  expect_equal(integrate_profile(example_vp, alpha=30)$mtr,-integrate_profile(example_vp, alpha=30+180)$mtr)
  expect_equal(integrate_profile(example_vpts, alpha=0)$mt,-integrate_profile(example_vpts, alpha=180)$mt)
})

