vpts <- example_vpts

test_that("filter_vpts() returns error on incorrect parameters", {
  expect_error(filter_vpts("not_a_vpts"))
  expect_error(filter_vpts(example_vp, min = "2016-09-01 00:02:00"))
  expect_error(filter_vpts(vpts, min = "not_a_date"))
  expect_error(filter_vpts(vpts, max = "not_a_date"))
  expect_error(filter_vpts(vpts, night = "not_a_boolean"))
})

test_that("filter_vpts() returns warning on out of bounds date", {
  expect_warning(filter_vpts(vpts, min = "2016-09-20 00:00:00"))
  expect_warning(filter_vpts(vpts, max = "2016-08-01 00:00:00"))
})

test_that("filter_vpts() returns the correct class of object", {
  expect_s3_class(filter_vpts(vpts, min = "2016-09-03 00:02:00"), "vpts")
})

test_that("filter_vpts() returns a non empty vpts (when there is data)", {
  expect_gt(length(filter_vpts(vpts, min = "2016-09-03 00:02:00")), 0)
})

test_that("filter_vpts() returns only night or only day correctly", {
  night_vpts <- filter_vpts(
    vpts, min = "2016-09-04 05:00:00", max = "2016-09-04 12:00:00", night = TRUE
  )
  day_vpts <- filter_vpts(
    vpts, min = "2016-09-04 05:00:00", max = "2016-09-04 12:00:00", night = FALSE
  )
  expect_true(
    unique(bioRad::check_night(
      night_vpts$datetime,
      night_vpts$attributes$where$lon,
      night_vpts$attributes$where$lat
    ))
  )
  expect_false(
    unique(bioRad::check_night(
      day_vpts$datetime,
      day_vpts$attributes$where$lon,
      day_vpts$attributes$where$lat
    ))
  )
})
