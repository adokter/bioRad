test_that("bind_into_vpts() returns error on incorrect parameters", {

})

test_that("vpts based on vp, multiple vp or vector of vp", {
  data(example_vpts)
  expect_s3_class(bind_into_vpts(example_vpts[1]), "vpts")
  expect_s3_class(bind_into_vpts(example_vpts[1], example_vpts[2]), "vpts")
  expect_s3_class(bind_into_vpts(c(example_vpts[1], example_vpts[2], example_vpts[3])), "vpts")
})

test_that("vpts based on vpts or multiple vpts", {
  data(example_vpts)
  expect_s3_class(bind_into_vpts(example_vpts[1:10]), "vpts")
  expect_s3_class(bind_into_vpts(example_vpts[1:10], example_vpts[11:20]), "vpts")
})
