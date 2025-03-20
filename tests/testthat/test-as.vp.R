test_that("as.vp() returns valid data", {
  df <- as.data.frame(example_vp, suntime=FALSE)
  expect_s3_class(as.vp(df), "vp")
  df <- as.data.frame(example_vpts, suntime=FALSE)
  expect_error(as.vp(df),"not a single vertical profile")
})
