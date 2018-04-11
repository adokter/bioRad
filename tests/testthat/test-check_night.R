context("test-check_night.R")

test_that("check_night identifies night with numeric values", {
  expect_false(check_night(5, 53, "2016-01-01 12:00"))
})

test_that("check_night identifies day/night for example data", {
  expect_is(check_night(VP), "logical")
  expect_true(check_night(VP))

  expect_is(check_night(VPTS), "logical")
  expect_true(check_night(VPTS)[1])
  expect_false(check_night(VPTS)[145])
})

