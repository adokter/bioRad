vptsfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")

test_that("read_vpts() returns error on incorrect parameters", {
  empty_file <- tempfile()
  file.create(empty_file)

  expect_error(
    read_vpts(empty_file),
    regex = glue::glue("File {empty_file} is empty."),
    fixed = TRUE
  )
  expect_error(
    read_vpts(tempfile()),
    regex = glue::glue("doesn't exist.")
  )
  expect_error(
    read_vpts(vptsfile,
              lat = -180),
    regexp = "'lat' should be numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
              lat = 180),
    regexp = "'lat' should be numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
              lat = "a"),
    regexp = "'lat' should be a single numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
              lat = c(48,32)),
    regexp = "'lat' should be a single numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
              lon = -180),
    regexp = 'argument "lat" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
              lon = -1080,
              lat = 38),
    regexp = "lon' should be numeric between -360 and 360 degrees",
    fixed = TRUE
  )
})
