test_that("read_vpfiles() returns error on incorrect parameters", {

})

test_that("Read a vertical profile", {
  vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
  expect_s3_class(read_vpfiles(vpfile), "vp")
  expect_s3_class(read_vpfiles(vpfile), "vp")
})

test_that("Read multiple vertical profiles", {
  vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
  # old function -> to vplist class; readvp.list was the function in the earlier
  # releases (not the S3 based method dispatching)
  expect_is(read_vpfiles(c(vpfile, vpfile)), "list")
  # new function -> just a list of vp
  expect_is(read_vpfiles(c(vpfile, vpfile)), "list")
  # expect_error(is(read_vpfiles(c(vpfile, vpfile)), "vplist")) # to add at end of refactoring
  expect_s3_class(read_vpfiles(c(vpfile, vpfile))[[1]], "vp")
  expect_s3_class(read_vpfiles(c(vpfile, vpfile))[[2]], "vp")
})
