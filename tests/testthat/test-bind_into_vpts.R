context("test-bind_into_vpts.R")

test_that("vpts based on vp, multiple vp or vector of vp", {
  data(VPTS)
  expect_s3_class(bind_into_vpts(VPTS[1]), "vpts")
  expect_s3_class(bind_into_vpts(VPTS[1], VPTS[2]), "vpts")
  expect_s3_class(bind_into_vpts(c(VPTS[1], VPTS[2], VPTS[3])), "vpts")
})

test_that("vpts based on vpts or multiple vpts", {
  data(VPTS)
  expect_s3_class(bind_into_vpts(VPTS[1:10]), "vpts")
  expect_s3_class(bind_into_vpts(VPTS[1:10], VPTS[11:20]), "vpts")
})
