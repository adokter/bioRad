hdf5_local_1 <- ""
hdf5_local_2 <- ""
hdf5_local_other_radar <- ""
vpts_gz_remote_1 <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202303.csv.gz"
vpts_gz_remote_2 <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202304.csv.gz"
vpts_gz_other_radar <- "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bewid/2023/bewid_vpts_202303.csv.gz"
# TODO: create the other vpts files from the files above, by downloading and gzipping

test_that("read_vpts() returns error on incorrect parameters", {

})

test_that("read_vpts() returns error on mixed extensions", {

})

test_that("read_vpts() can read local vp hdf5 files", {
  # Test for one file
  # Test for multiple files
  # Returns vpts class
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
