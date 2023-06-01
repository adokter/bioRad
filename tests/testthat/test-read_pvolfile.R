pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")

test_that("read_pvolfile() returns error on incorrect parameters", {
  expect_error(
    read_pvolfile("not_a_filename"),
    regexp= "does not exist in current working directory"
  )
  expect_error(
    read_pvolfile(pvolfile, sort = 1),
    regexp = "'sort' should be logical",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, sort = c(TRUE,TRUE)),
    regexp = "'sort' should be logical",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, lat = 173),
    regexp = "'lat' should be numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, lat = -173),
    regexp = "'lat' should be numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, lat = "not_a_latitude"),
    regexp = "'lat' should be numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, lat = c(71,83)),
    regexp = "'lat' should be numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, lon = "not_a_longitude"),
    regexp = "'lon' should be numeric between -360 and 360 degrees",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, lon = -4893),
    regexp = "'lon' should be numeric between -360 and 360 degrees",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, lon = 4E9),
    regexp = "'lon' should be numeric between -360 and 360 degrees",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, lon = c(42,36)),
    regexp = "'lon' should be numeric between -360 and 360 degrees",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, height = -12),
    regexp = "'height' should be a positive number of meters above sea level",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, height = "not_an_integer"),
    regexp = "'height' should be a positive number of meters above sea level",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, height = c(12,42)),
    regexp = "'height' should be a positive number of meters above sea level",
    fixed = TRUE
  )
  expect_error(
    read_pvolfile(pvolfile, height = -10),
    regexp = "'height' should be a positive number of meters above sea level",
    fixed = TRUE
  )

  temp_dir <- tempdir()
  rhdf5::h5createFile(file.path(temp_dir,"missing_groups.h5"))

  expect_error(
    suppressWarnings(read_pvolfile(file.path(temp_dir,"missing_groups.h5"))),
    regexp = "Failed to read HDF5 file.",
    fixed = TRUE
  )
})
