# Load exaymple vp and vpts
vpts <- example_vpts
vp <- example_vp


library(bioRad)
source('R/integrate_profile.R')

test_that("Integrate_profile() return vpi", {
  expect_s3_class(integrate_profile(vp), "vpi")
  expect_s3_class(integrate_profile(c(vp,vp)), "vpi")
  expect_s3_class(integrate_profile(vpts), "vpi")
})

test_that("integrate_profile() returns error on incorrect parameters", {
  expect_error(integrate_profile(vp, alt_min = 'notANumeric'))
  expect_error(integrate_profile(vp, alt_max = 'notANumeric'))
  expect_error(integrate_profile(vp, alpha = 'notANumeric'))
  expect_error(integrate_profile(vp, alt_min = 500, alt_max = 300))
})

test_that("integrate_profile() returns same value for any alt_min below height", {
  expect_equal(integrate_profile(vp, alt_min = 0)$height, integrate_profile(vp, alt_min = vp$attributes$where$height)$height)
  expect_equal(integrate_profile(vpts, alt_min = 0)$height, integrate_profile(vpts, alt_min = vpts$attributes$where$height)$height)
})
