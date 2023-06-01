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
      lat = -180
    ),
    regexp = "'lat' should be numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      lat = 180
    ),
    regexp = "'lat' should be numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      lat = "a"
    ),
    regexp = "'lat' should be a single numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      lat = c(48, 32)
    ),
    regexp = "'lat' should be a single numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      lat = 66
    ),
    regexp = "'radar' argument missing. Required to specify a radar identifier.",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      lon = -180
    ),
    regexp = "'radar' argument missing. Required to specify a radar identifier.",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      lon = -1080,
      lat = 38
    ),
    regexp = "lon' should be numeric between -360 and 360 degrees",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      height = -1
    ),
    regexp = "'height' should be a positive number of meters above sea level",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      height = NA
    ),
    regexp = "height is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      height = NA
    ),
    regexp = "height is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    read_vpts(vptsfile,
      radar = "KBGM",
      height = seq(50)
    ),
    regexp = "height is not a number (a length one numeric vector).",
    fixed = TRUE
  )
})
