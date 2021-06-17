pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
vpfile <- system.file("extdata", "profile.h5", package = "bioRad")
textfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")
ghostfile <- paste(tempdir(), "garbage", sep = "/")

# No tests for error on incorrect parameters:
# is.pvolfile() returns TRUE/FALSE and works for every input

test_that("is.pvolfile() can identify ODIM HDF5 polar volume files", {
  expect_true(is.pvolfile(pvolfile))
  expect_false(is.pvolfile(vpfile))
  expect_false(is.pvolfile(textfile))
  expect_warning(is.pvolfile(ghostfile), "Can't find") # From get_odim_object_type
  # TODO: test and identify NEXRAD and IRIS polar volume files
})

test_that("get_odim_object_type() returns a valid ODIM type", {
  # Type is PVOL, VP or NA
  expect_equal(get_odim_object_type(pvolfile), "PVOL")
  expect_equal(get_odim_object_type(vpfile), "VP")
  expect_equal(get_odim_object_type(textfile), NA)
  expect_equal(get_odim_object_type(ghostfile), NA)
})

test_that("get_odim_object_type() returns warnings", {
  # Note: these are not errors
  expect_warning(get_odim_object_type(ghostfile), "Can't find")
  expect_warning(get_odim_object_type(textfile), "is not an HDF5 file")

  # Helper function to make invalid files
  delete_group <- function(vpfile, group) {
    filename <- glue("{tempdir()}/vp_invalid.h5")
    file.copy(vpfile, filename, overwrite = TRUE)
    h5delete(file = filename, name = group)
    filename
  }
  expect_warning(get_odim_object_type(delete_group(vpfile, "dataset1")), "does not contain a `/dataset1` group")
  expect_warning(get_odim_object_type(delete_group(vpfile, "what")), "does not contain a `/what` group")
  expect_warning(get_odim_object_type(delete_group(vpfile, "where")), "does not contain a `/where` group")
  expect_warning(get_odim_object_type(delete_group(vpfile, "how")), "does not contain a `/how` group")
})
