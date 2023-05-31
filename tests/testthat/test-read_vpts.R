hdf5_local_vp_1 <- "https://lw-enram.s3-eu-west-1.amazonaws.com/be/jab/2016/09/19/23/bejab_vp_201609192315.h5"
hdf5_local_vp_2 <- "https://lw-enram.s3-eu-west-1.amazonaws.com/be/jab/2016/09/19/23/bejab_vp_201609192320.h5"
hdf5_local_vp_other_radar <- ""


vpts_gz_remote_1 <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202303.csv.gz"
vpts_gz_remote_2 <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202304.csv.gz"
vpts_gz_other_radar <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bewid/2023/bewid_vpts_202303.csv.gz"


# TODO: create the other vpts files from the files above, by downloading and gzipping
test_that("read_vpts() returns error on incorrect parameters", {

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
    expect_true(length(result) == 1, "Expected one vp object to be returned when reading one file.")

    # Test if the output is a vpts object
    expect_true(is.vpts(result), "Coverstion to vpts object failed.")

    file.remove(temp_file)
  }
  
  # Test for multiple files
  temp_files <- lapply(urls, function(url) {
    temp_file <- tempfile()
    curl::curl_download(url, destfile = temp_file)
    temp_file
  })
  result <- read_vpts_hdf5(temp_files)
  expect_true(length(result) == n, paste("Expected", n, "vp objects to be returned when reading", n, "files."))

  # Test if the output is a vpts object
  expect_true(all(sapply(result, is.vpts)), "Conversion to vpts object failed.")

  # Remove temporary files
  for (temp_file in temp_files) {
    file.remove(temp_file)
  }
})


test_that("read_vpts() returns error on multiple radars in vp hdf5 files", {

})

test_that("read_vpts() can read local (gzipped) VPTS CSV files", {
  skip_if_offline()
  # Test for one file
  # Test for multiple files
  # Returns vpts class
})

test_that("read_vpts() can read remote (gzipped) VPTS CSV files", {
  skip_if_offline()
  # Test for one file
  # Test for multiple files
  # Returns vpts class
})

test_that("read_vpts() returns error on multiple radars in VPTS CSV files", {
  skip_if_offline()
  # Note: this is a limitation until we switch to vpts data frame objects

})
