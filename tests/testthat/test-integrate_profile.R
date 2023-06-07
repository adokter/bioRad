test_that("Integrate_profile() return vpi", {
  expect_s3_class(integrate_profile(example_vp), "vpi")
  expect_s3_class(integrate_profile(c(example_vp,example_vp)), "vpi")
  expect_s3_class(integrate_profile(example_vpts), "vpi")
})

test_that("integrate_profile() returns error on incorrect parameters", {
  expect_error(
    integrate_profile(example_vp, alt_min = 'notANumeric'),
    regexp = 'alt_min is not a number (a length one numeric vector). or alt_min not equal to "antenna"',
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vp, alt_max = 'notANumeric'),
    regexp = "alt_max is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vp, alt_max = c(pi, 4)),
    regexp = "alt_max is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    integrate_profile(example_vp, alpha = 'notANumeric'),
    regexp = "alpha is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vpts, interval_max = 'notANumeric'),
    regexp = "interval_max is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vpts, interval_replace = 'notANumeric'),
    regexp = "interval_replace is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vp, alt_min = 500, alt_max = 300),
    regexp = "'alt_min' should be smaller than 'alt_max'",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vp, height_quantile = "a"),
    regexp = "height_quantile is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vp, height_quantile = 2),
    regexp = "height_quantile not less than 1",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vpts, height_quantile = "a"),
    regexp = "height_quantile is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vpts, height_quantile = 2),
    regexp = "height_quantile not less than 1",
    fixed = TRUE
    )
  expect_error(
    integrate_profile(example_vp, alt_max = 2, alt_min = 3)
    ,
               regexp = "'alt_min' should be smaller than 'alt_max'",
               fixed = TRUE)
  expect_error(integrate_profile(example_vpts, alt_max = 2, alt_min = 3),
               regexp = "'alt_min' should be smaller than 'alt_max'",
               fixed = TRUE)
})

test_that("integrate_profile() returns error on non vp object in list input",{
  expect_error(integrate_profile(x = list(example_vp, example_vpts)),
               regexp = "requires list of vp objects as input",
               fixed = TRUE)
})

test_that("integrate_profile() checks value of interval_max", {
  expect_warning(integrate_profile(example_vpts, interval_max = 1),
                 regexp = "interval_max < median timestep of the time series (300 sec), consider a larger value.",
                 fixed = TRUE)
  expect_warning(integrate_profile(example_vpts, interval_max = 1, interval_replace=10),
                 regexp = "interval_max < median timestep of the time series (300 sec), consider a larger value.",
                 fixed = TRUE)
})

test_that("rotating transect by 180 degrees results in sign flip", {
  expect_equal(integrate_profile(example_vp, alpha=30)$mtr,-integrate_profile(example_vp, alpha=30+180)$mtr)
  expect_equal(integrate_profile(example_vpts, alpha=0)$mt,-integrate_profile(example_vpts, alpha=180)$mt)
})

