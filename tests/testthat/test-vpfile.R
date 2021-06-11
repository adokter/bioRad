pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
vpfile <- system.file("extdata", "profile.h5", package = "bioRad")

test_that("is.vpfile() can identify ODIM HDF5 vertical profile files", {
  expect_true(is.vpfile(vpfile))
  expect_false(is.vpfile(pvolfile))
  expect_warning(is.vpfile("not_a_file"), "Can't find") # From get_odim_object_type
})
