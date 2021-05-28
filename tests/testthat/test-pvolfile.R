pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
textfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")
ghostfile <- paste(tempdir(), "/garbage", sep = "")

# No tests for error on incorrect parameters:
# is.pvolfile() returns TRUE/FALSE and works for every input

test_that("get_odim_object_type() returns a valid ODIM type", {
  expect_equal(get_odim_object_type(pvolfile), "PVOL")
  expect_equal(get_odim_object_type(vpfile), "VP")
  expect_equal(suppressWarnings(get_odim_object_type(textfile)), NA)
  expect_warning(get_odim_object_type(textfile), "is not a HDF5 file.")
  expect_equal(suppressWarnings(get_odim_object_type(ghostfile)), NA)
  expect_warning(get_odim_object_type(ghostfile), "does not exist.")
})

test_that("is.pvolfile() can identify ODIM hdf5 polar volume files", {
  expect_true(is.pvolfile(pvolfile))
  expect_false(is.pvolfile(vpfile))
  expect_false(suppressWarnings(is.pvolfile(textfile)))
  expect_warning(is.pvolfile(ghostfile), "does not exist.")
  # TODO: test and identify NEXRAD and IRIS polar volume files
})
