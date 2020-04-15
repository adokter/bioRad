#locate a polar volume file:
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
textfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")
ghostfile <- paste(tempdir(),"/garbage",sep="")

test_that("is.odimfile() can identify ODIM hdf5 files", {
  expect_equal(bioRad:::is.odimfile(pvolfile), TRUE)
  expect_equal(bioRad:::is.odimfile(vpfile), TRUE)
  expect_equal(suppressWarnings(bioRad:::is.odimfile(textfile)), FALSE)
  # TODO: test for non-complient ODIM hdf5 files with missing groups/attributes
})

test_that("get_odim_object_type() returns a valid ODIM type", {
  expect_equal(get_odim_object_type(pvolfile),"PVOL")
  expect_equal(get_odim_object_type(vpfile),"VP")
  expect_equal(suppressWarnings(get_odim_object_type(textfile)),NA)
  expect_warning(get_odim_object_type(textfile),paste(textfile, "is not a HDF5 file"))
  expect_equal(suppressWarnings(get_odim_object_type(ghostfile)),NA)
  expect_warning(get_odim_object_type(ghostfile),paste(ghostfile, "does not exist"))
})

test_that("is.pvolfile() can identify ODIM hdf5 polar volume files", {
  expect_equal(is.pvolfile(pvolfile), TRUE)
  expect_equal(is.pvolfile(vpfile), FALSE)
  expect_equal(is.pvolfile(textfile), FALSE)
  expect_warning(is.pvolfile(ghostfile),paste(ghostfile, "does not exist"))
  # TODO: test and identify NEXRAD and IRIS polar volume files
})
