context("test-read_vpfiles.R")

test_that("Read a vertical profile", {
  prof <- system.file("extdata", "profile.h5", package = "bioRad")
  expect_s3_class(readvp(prof), "vp")
  expect_s3_class(read_vpfiles(prof), "vp")
})

test_that("Read a list of vertical profiles", {
  prof <- system.file("extdata", "profile.h5", package = "bioRad")
  # old function -> to vplist class; readvp.list was the function in the earlier
  # releases (not the S3 based method dispatching)
  expect_s3_class(readvp.list(c(prof, prof)), "vplist")
  expect_is(readvp.list(c(prof, prof)), "list")
  # new function -> just a list of vp
  expect_is(read_vpfiles(c(prof, prof)), "list")
  expect_s3_class(read_vpfiles(c(prof, prof))[[1]], "vp")
  expect_s3_class(read_vpfiles(c(prof, prof))[[2]], "vp")
})
