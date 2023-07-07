# Define the URLs of test files
urls <- c(
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/hdf5/czbrd/2023/06/01/czbrd_vp_20230601T000000Z_0xb.h5",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/hdf5/czbrd/2023/06/01/czbrd_vp_20230601T000500Z_0xb.h5",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/hdf5/czbrd/2023/06/01/czbrd_vp_20230601T001000Z_0xb.h5",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202303.csv.gz",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202304.csv.gz",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bewid/2023/bewid_vpts_202303.csv.gz"
)


# Define the path to the new temporary directory
temp_dir <- tempdir()

# Create the new directory if not exists
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

# Create the h5 and csv sub-directories
h5_dir <- file.path(temp_dir, "h5")
csv_dir <- file.path(temp_dir, "csv")
other_dir <- file.path(temp_dir, "other")

if (!dir.exists(h5_dir)) dir.create(h5_dir)
if (!dir.exists(csv_dir)) dir.create(csv_dir)
if (!dir.exists(other_dir)) dir.create(other_dir)

sapply(urls, function(url) download_test_file(url, temp_dir, h5_dir, csv_dir))

# Define the paths to subdirectories  within temp directory
temp_h5_dir <- file.path(temp_dir, "h5")
h5_files <- list.files(temp_h5_dir, pattern = "*.h5", full.names = TRUE)

temp_gz_dir <- file.path(temp_dir, "csv")
gz_files <- list.files(temp_gz_dir, pattern = "*.gz", full.names = TRUE)

# Expect rerouting to read_stdout() with previous arguments
test_that("read_vpts correctly throws deprecation warning and reroutes to read_stdout", {
  vptsfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")

  expect_warning(
    read_vpts(file = vptsfile, radar = "KGBM", lat = 12, lon = 34, height = 1000, wavelength = "S"),
    "read_stdout"
  )

  # test read_stdout() txt without explicit extension
  no_ext_file <- tempfile(pattern = "example_vpts")
  file.copy(from = vptsfile, to = no_ext_file)

  expect_warning(
    read_vpts(file = no_ext_file, radar = "KBGM", lat = 12, lon = 34, height = 1000, wavelength = "S"),
    "read_stdout"
  )

  expect_warning(
    read_vpts(file = vptsfile, radar = "KBGM", lat = 12, lon = 34, height = 1000, wavelength = "S"),
    "read_stdout"
  )

  # Test if outputs from both functions are equal but suppress warnings in tests
  suppressWarnings(expect_equal(
    read_vpts(files = vptsfile, radar = "radar", lat = 12, lon = 34, height = 1000),
    read_stdout(file = vptsfile, radar = "radar", lat = 12, lon = 34, height = 1000, wavelength = "C", sep = "")
  ))
})

test_that("read_vpts() returns error on explicit mixed extensions", {
  # Prepare a vector of file paths with different extensions
  files <- c("file1.csv", "file2.gz")
  # Expect an error when calling read_vpts() with this input
  expect_error(read_vpts(files), "`files` must all have the same extension.")
})

test_that("read_vpts() returns on error on inferred mixed extensions", {
  # test an h5 file with a csv extension
  example_h5 <- h5_files[1]
  new_filename <- paste0(tools::file_path_sans_ext(basename(example_h5)), ".csv")
  new_filepath <- file.path(other_dir, new_filename)
  file.copy(example_h5, new_filepath)

  expect_error(read_vpts(new_filepath), "does not match the guessed file type")
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
  # add eehar h5
  eehar <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/hdf5/eehar/2023/06/01/eehar_vp_20230601T001000Z_0xb.h5"
  download_test_file(eehar, temp_dir, h5_dir, csv_dir)

  h5_files <- list.files(temp_h5_dir, pattern = "*.h5", full.names = TRUE)

  expect_warning(
    read_vpts(h5_files),
    "profiles are not from a single radar"
  )

  file.remove(file.path(h5_dir, basename(eehar)))
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


test_that("check ability to convert a vpts object into a data.frame, and then cast it back into a vpts", {
  vptsfile <- system.file("extdata", "example_vpts.csv", package = "bioRad")
  my_vpts <- read_vpts(vptsfile)
  res <- as.vpts(as.data.frame(my_vpts))
  expect_true(is.vpts(res))
})

# Comapre read_vpts output from data in both formats

test_that("read_vpts() returns equal summaries from h5 and csv files from 1 day of data", {
  skip_if_offline()

  # clear directories
  file.remove(list.files(h5_dir, full.names = TRUE))
  file.remove(list.files(csv_dir, full.names = TRUE))

  # h5
  prefixes <- c("baltrad/hdf5/bewid/2023/04/14")

  # Loop over the prefixes
  for (prefix in prefixes) {
    message("Starting download for prefix:", prefix)

    # Get the files for the current prefix
    h5_files <- aws.s3::get_bucket_df(
      bucket = "s3://aloft/",
      prefix = prefix,
      region = "eu-west-1"
    )

    # Download the files to the temporary directory
    sapply(h5_files$Key, function(file_name) {
      aws.s3::save_object(
        file = paste0(h5_dir, "/", basename(file_name)),
        object = file_name,
        bucket = "s3://aloft/",
        region = "eu-west-1"
      )
    })

    message("Completed download for prefix:", prefix)
  }

  h5_files <- list.files(h5_dir, full.names = TRUE)

  # VPTS CSV

  urls <- c("https://aloft.s3-eu-west-1.amazonaws.com/baltrad/daily/bewid/2023/bewid_vpts_20230414.csv")

  # Use lapply to download each file to a temporary location
  csv_files <- lapply(urls, function(url) {
    file_name <- basename(url)
    temp_file <- file.path(csv_dir, file_name)
    curl::curl_download(url, temp_file)
    return(temp_file)
  })

  my_vpts_csv <- read_vpts(unlist(csv_files))
  my_vpts_h5 <- read_vpts(h5_files)

  # Expect equivalent summaries from both vpts objects
  expect_equal(summary(my_vpts_csv), summary(my_vpts_h5))
})
