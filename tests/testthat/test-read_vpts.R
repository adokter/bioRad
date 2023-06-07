
# Define the paths to subdirectories  within temp directory
temp_h5_dir <- file.path(temp_dir, "h5")
h5_files <- list.files(temp_h5_dir, pattern = "*.h5", full.names = TRUE)

temp_gz_dir <- file.path(temp_dir, "csv")
gz_files <- list.files(temp_gz_dir, pattern = "*.gz", full.names = TRUE)

# Expect rerouting to read_stdout() with previous arguments
test_that("read_vpts correctly throws deprecation warning and reroutes to read_stdout", {
  vptsfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")

  expect_warning(
    read_vpts(file = vptsfile, radar = "radar", lat = 12, lon = 34, height = 1000),
    "deprecated"
  )

  expect_error(
    read_vpts(file = vptsfile),
    "'radar' argument missing. Required to specify a radar identifier."
  )

  # Test if outputs from both functions are equal but supress warnings in tests
  suppressWarnings(expect_equal(
    read_vpts(files = vptsfile, radar = "radar", lat = 12, lon = 34, height = 1000),
    read_stdout(file = vptsfile, radar = "radar", lat = 12, lon = 34, height = 1000, wavelength = "C", sep = "")
  ))
})

test_that("read_vpts() returns error on mixed extensions", {
  # Prepare a vector of file paths with different extensions
  files <- c("file1.csv", "file2.gz")
  # Expect an error when calling read_vpts() with this input
  expect_error(read_vpts(files), "`files` must all have the same extension.")
})

test_that("read_vpts() can read local vp hdf5 files", {

  # Test for one file
  {
    result <- read_vpts_hdf5(h5_files[1])

    expect_true(
      length(result$datetime) == 1,
      "Expected one vp object to be returned when reading one file."
    )
    # Test if the output is a vpts object
    expect_true(is.vpts(result))
  }

  # Test for multiple files
  {
    result <- read_vpts_hdf5(h5_files[1:2])

    expect_true(
      length(result$datetime) == 2,
      "Expected two vp objects to be returned when reading two files."
    )
    # Test if the output is a vpts object
    expect_true(is.vpts(result))
  }
})

test_that("read_vpts() returns error on multiple radars in vp hdf5 files", {
  skip("Not yet implemented")

  h5_files <- list.files(temp_h5_dir, pattern = "*.h5", full.names = TRUE)

  expect_error(
    read_vpts(h5_files),
    "`files` must contain data of a single radar."
  )
})

test_that("read_vpts() can read remote (gzipped) VPTS CSV files", {
  skip_if_offline()

  gz_urls <- urls[grepl("\\.gz$", urls)]

  # Test for one file
  result <- read_vpts(gz_urls[1])

  # Returns vpts class
  expect_true(is.vpts(result))

  # Test for multiple files
  result <- read_vpts(gz_urls[1:2])

  # Returns vpts class
  expect_true(is.vpts(result))
})

test_that("read_vpts() can read local (gzipped) VPTS CSV files", {
  skip_if_offline()

  # Test for one file
  result <- read_vpts(gz_files[1])

  ## Returns vpts class
  expect_true(is.vpts(result))

  # Test for multiple files
  result <- read_vpts(gz_files[1:2])

  # Returns vpts class
  expect_true(is.vpts(result))
})

test_that("read_vpts() returns error on multiple radars in VPTS CSV files", {
  skip_if_offline()
  # Note: this is a limitation until we switch to vpts data frame objects

  expect_error(
    read_vpts(gz_files),
    "`files` must contain data of a single radar."
  )
})

# clean up
unlink(temp_dir, recursive = TRUE)
