hdf5_local_vp_1 <- "https://lw-enram.s3-eu-west-1.amazonaws.com/be/jab/2016/09/19/23/bejab_vp_201609192315.h5"
hdf5_local_vp_2 <- "https://lw-enram.s3-eu-west-1.amazonaws.com/be/jab/2016/09/19/23/bejab_vp_201609192320.h5"
hdf5_local_vp_other_radar <- "https://lw-enram.s3-eu-west-1.amazonaws.com/cz/brd/2016/09/19/00/czbrd_vp_20160919T0000Z_0x5.h5"

vpts_gz_remote_1 <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202303.csv.gz"
vpts_gz_remote_2 <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202304.csv.gz"
vpts_gz_other_radar <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bewid/2023/bewid_vpts_202303.csv.gz"


# TODO: create the other vpts files from the files above, by downloading and gzipping

# Expect rerouting to read_stdout() with previous arguments
test_that("read_vpts correctly throws deprecation warning and reroutes to read_stdout", {
  vptsfile <- system.file("extdata", "example_vpts.txt", package = "bioRad")

  expect_warning(
    read_vpts(file = vptsfile, radar = "radar", lat = 12, lon = 34, height = 1000),
    "deprecated"
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
  skip_if_offline()

  urls <- c(hdf5_local_vp_1, hdf5_local_vp_2)
  n <- length(urls)

  # Test for one file
  {
    temp_file <- tempfile()
    curl::curl_download(urls[1], destfile = temp_file)
    result <- read_vpts_hdf5(temp_file)

    expect_true(length(result$datetime) == 1, "Expected one vp object to be returned when reading one file.")
    # Test if the output is a vpts object
    expect_true(is.vpts(result))

    file.remove(temp_file)
  }

  # Test for multiple files
  temp_files <- lapply(urls, function(url) {
    temp_file <- tempfile()
    curl::curl_download(url, destfile = temp_file)
    temp_file
  })
  result <- read_vpts_hdf5(temp_files)
  expect_true(length(result$datetime) == n, paste("Expected", n, "vp objects to be returned when reading", n, "files."))

  # Test if the output is a vpts object
  expect_true(is.vpts(result))

  # Remove temporary files
  for (temp_file in temp_files) {
    file.remove(temp_file)
  }
})

test_that("read_vpts() returns error on multiple radars in vp hdf5 files", {
  skip("Not yet implemented")

  urls <- c(hdf5_local_vp_1, hdf5_local_vp_2, hdf5_local_vp_other_radar)

  temp_files <- lapply(urls, function(url) {
    temp_file <- tempfile()
    curl::curl_download(url, destfile = temp_file)
    temp_file
  })

  expect_error(
    read_vpts(temp_files),
    "`files` must contain data of a single radar."
  )
})

test_that("read_vpts() can read remote (gzipped) VPTS CSV files", {
  skip_if_offline()

  # Test for one file
  url <- vpts_gz_remote_1
  result <- read_vpts(url)

  # Returns vpts class
  expect_true(is.vpts(result))

  # Test for multiple files
  urls <- c(vpts_gz_remote_1, vpts_gz_remote_2)
  result <- read_vpts(urls)

  # Returns vpts class
  expect_true(is.vpts(result))
})

test_that("read_vpts() can read local (gzipped) VPTS CSV files", {
  skip_if_offline()

  # Test for one file
  url <- vpts_gz_remote_1

  temp_dir <- tempdir()
  file_name <- basename(url)
  dest_file <- file.path(temp_dir, file_name)
  curl::curl_download(url, destfile = dest_file)
  result <- read_vpts(dest_file)

  # Returns vpts class
  expect_true(is.vpts(result))

  # Test for multiple files
  urls <- c(vpts_gz_remote_1, vpts_gz_remote_2)

  temp_files <- sapply(urls, function(url) {
    temp_dir <- tempdir()
    file_name <- basename(url)
    dest_file <- file.path(temp_dir, file_name)
    curl::curl_download(url, destfile = dest_file)
    return(dest_file)
  }, simplify = TRUE)

  # Returns vpts class
  expect_true(is.vpts(result))
})

test_that("read_vpts() returns error on multiple radars in VPTS CSV files", {
  skip_if_offline()
  # Note: this is a limitation until we switch to vpts data frame objects
  urls <- c(vpts_gz_remote_1, vpts_gz_remote_2, vpts_gz_other_radar)

  temp_dir <- tempdir()

  temp_files <- sapply(urls, function(url) {
    # Download the gzip file

    file_name <- basename(url)
    dest_file <- file.path(temp_dir, file_name)

    if (!file.exists(dest_file)) {
      curl::curl_download(url, destfile = dest_file)
    }

    return(dest_file)
  }, simplify = TRUE)


  expect_error(
    read_vpts(temp_files),
    "`files` must contain data of a single radar."
  )

  unlink(temp_dir, recursive = TRUE)
})
