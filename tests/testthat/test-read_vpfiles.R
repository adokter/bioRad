context("test-read_vpfiles.R")

test_that("Read a vertical profile file", {
  prof <- system.file("extdata", "profile.h5", package = "bioRad")
  expect_s3_class(readvp(prof), "vp")
  expect_s3_class(read_vpfiles(prof), "vp")
})

test_that("Read a list of vertical profile files", {
  prof <- system.file("extdata", "profile.h5", package = "bioRad")
  # old function -> to vplist class
  expect_s3_class(readvp.list(c(prof, prof)), "vplist")
  expect_is(readvp.list(c(prof, prof)), "list")
  # new function -> just a list of vp
  expect_is(read_vpfiles(c(prof, prof)), "list")
  expect_s3_class(read_vpfiles(c(prof, prof))[[1]], "vp")
  expect_s3_class(read_vpfiles(c(prof, prof))[[2]], "vp")
})
