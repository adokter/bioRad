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



test_that("vp with different heights", {
  data(example_vp)
  lower <- example_vp
  lower$data <- lower$data[1:20, ]
  higher <- example_vp
  higher$data <- higher$data[3:25, ]
  expect_s3_class(bind_into_vpts(example_vp, lower), "vpts")
  expect_s3_class(bind_into_vpts(list(example_vp, higher)), "vpts")
  expect_s3_class(bind_into_vpts(example_vp, higher), "vpts")
  higher$data$height <- higher$data$height + 1
  expect_error(bind_into_vpts(example_vp, higher))
})
test_that("vpts with different heights", {
  data(example_vpts)
  lower <- example_vpts
  lower$height <- lower$height[1:20]
  lower$data <- lapply(lower$data, head, 20)
  expect_s3_class(bind_into_vpts(lower, example_vpts),'vpts')
})
