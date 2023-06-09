vptsfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")

test_that("read_stdout() returns error on incorrect parameters", {
  empty_file <- tempfile()
  file.create(empty_file)

  expect_error(
    read_stdout(empty_file),
    regex = glue::glue("File {empty_file} is empty."),
    fixed = TRUE
  )
  expect_error(
    read_stdout(tempfile()),
    regex = glue::glue("doesn't exist.")
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", lat = -180),
    regexp = "'lat' should be a single numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", lat = 180),
    regexp = "'lat' should be a single numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", lat = "a"),
    regexp = "'lat' should be a single numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", lat = c(48, 32)),
    regexp = "'lat' should be a single numeric between -90 and 90 degrees",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, lat = 66),
    regexp = "'radar' argument missing. Required to specify a radar identifier.",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, lon = -180),
    regexp = "'radar' argument missing. Required to specify a radar identifier.",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", lon = -1080, lat = 38),
    regexp = "'lon' should be a single numeric between -360 and 360 degrees",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", height = -1),
    regexp = "'height' should be a single positive number of meters above sea level",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", height = NA),
    regexp = "'height' should be a single positive number of meters above sea level",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", height = NA),
    regexp = "'height' should be a single positive number of meters above sea level",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, radar = "KBGM", height = seq(50)),
    regexp = "'height' should be a single positive number of meters above sea level",
    fixed = TRUE
  )

  wavelength_msg <-
    glue::glue(
      "'wavelength' should be a single positive number",
      ", or one of 'C' or 'S' for C-band and S-band radar, respectively."
    )
  expect_error(
    read_stdout(vptsfile, wavelength = "a", radar = "KBGM"),
    regexp = wavelength_msg,
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, wavelength = -12, radar = "KBGM"),
    regexp = wavelength_msg,
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, wavelength = 1:3, radar = "KBGM"),
    regexp = wavelength_msg,
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, wavelength = "Q", radar = "KBGM"),
    regexp = wavelength_msg,
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, wavelength = "S", radar = "KBGM", sep = "|"),
    regexp = "'sep' should be either \",\" or \"\"",
    fixed = TRUE
  )
  expect_error(
    suppressWarnings(read_stdout(vptsfile, radar = "KBGM", sep = NA)),
    regexp = "'sep' should be either \",\" or \"\"",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, sep = c(",", "&"), wavelength = "C"),
    regexp = "'sep' should be either \",\" or \"\"",
    fixed = TRUE
  )
  expect_error(
    read_stdout(vptsfile, sep = c(",", "||")),
    regexp = "'sep' should be either \",\" or \"\"",
    fixed = TRUE
  )
})

test_that("read_stdout() warns for missing wavelength", {
  expect_warning(
    read_stdout(vptsfile, radar = "KBGM"),
    regexp = "No 'wavelength' argument provided, assuming radar operates at C-band",
    fixed = TRUE
  )
})
