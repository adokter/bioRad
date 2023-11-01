test_that("read_cajun() returns error on incorrect parameters", {
  # test for non existing file
  not_a_real_filepath <- file.path(tempdir(), "not_a_filename")
  expect_error(
    read_cajun(not_a_real_filepath), "doesn't exist")
  # test for an empty file
  empty_file_path <- file.path(tempfile())
  file.create(empty_file_path)
  expect_error(
    read_cajun(empty_file_path),"is empty.")
  # wavelength
  non_empty_file_path <- tempfile()
  cajun_csv <- dplyr::tribble(
    ~bin_lower, ~height, ~linear_eta, ~nbins, ~direction, ~speed, ~u, ~v, ~rmse, ~elev1, ~nvolumes_gr35_e1, ~elev2, ~nvolumes_gr35_e2, ~vcp, ~linear_eta_unfiltered, ~percent_rain,
    0, 50, 851.2421, 29911, 36.8916, 9.1082, 5.4677, 7.2845, 12.5202, 0.3646, 216, 1.3329, 0, 21, 852.3826, 0.005955744,
    100, 150, 212.6159, 49823, 44.6877, 14.4428, 10.1568, 10.2681, 11.5492, 0.3646, 216, 1.3329, 0, 21, 213.7921, 0.1159383
  )
  write.csv(x = cajun_csv, non_empty_file_path)
  expect_error(
    read_cajun(non_empty_file_path, wavelength = "a"),
    regexp = "Not a valid 'wavelength' argument.",
    fixed = TRUE
  )
  expect_error(
    read_cajun(non_empty_file_path, wavelength = 1:4),
    regexp = "Not a valid 'wavelength' argument.",
    fixed = TRUE
  )
  expect_error(
    read_cajun(non_empty_file_path, wavelength = c("C", "S")),
    regexp = "Not a valid 'wavelength' argument.",
    fixed = TRUE
  )
  expect_no_error(
    read_cajun(non_empty_file_path, wavelength = c("S"))
  )
  expect_no_error(
    read_cajun(non_empty_file_path, wavelength = c("C"))
  )
})

# TODO add more tests to validate file was read correctly
test_that("read_cajun() returns vp object", {
  non_empty_file_path <- tempfile()
  cajun_csv <- dplyr::tribble(
    ~bin_lower, ~height, ~linear_eta, ~nbins, ~direction, ~speed, ~u, ~v, ~rmse, ~elev1, ~nvolumes_gr35_e1, ~elev2, ~nvolumes_gr35_e2, ~vcp, ~linear_eta_unfiltered, ~percent_rain,
    0, 50, 851.2421, 29911, 36.8916, 9.1082, 5.4677, 7.2845, 12.5202, 0.3646, 216, 1.3329, 0, 21, 852.3826, 0.005955744,
    100, 150, 212.6159, 49823, 44.6877, 14.4428, 10.1568, 10.2681, 11.5492, 0.3646, 216, 1.3329, 0, 21, 213.7921, 0.1159383
  )
  write.csv(x = cajun_csv, non_empty_file_path)
  expect_s3_class(
    read_cajun(non_empty_file_path, wavelength = c("S")),
    "vp"
  )
})

test_that("read_cajun() returns expected data", {
  non_empty_file_path <- tempfile()
  cajun_csv <- dplyr::tribble(
    ~bin_lower, ~height, ~linear_eta, ~nbins, ~direction, ~speed, ~u, ~v, ~rmse, ~elev1, ~nvolumes_gr35_e1, ~elev2, ~nvolumes_gr35_e2, ~vcp, ~linear_eta_unfiltered, ~percent_rain,
    0, 50, 851.2421, 29911, 36.8916, 9.1082, 5.4677, 7.2845, 12.5202, 0.3646, 216, 1.3329, 0, 21, 852.3826, 0.005955744,
    100, 150, 212.6159, 49823, 44.6877, 14.4428, 10.1568, 10.2681, 11.5492, 0.3646, 216, 1.3329, 0, 21, 213.7921, 0.1159383
  )
  write.csv(x = cajun_csv, non_empty_file_path)
  expect_equal(
    ignore_attr = TRUE,
    tolerance=0.0001,
    read_cajun(non_empty_file_path, wavelength = c("S"))$data,
    dplyr::tribble(
      ~height, ~u, ~v, ~w, ~ff, ~dd, ~sd_vvp, ~gap, ~dbz, ~eta, ~dens, ~DBZH, ~n, ~n_dbz, ~n_all, ~n_dbz_all, ~elev1, ~nvolumes_gr35_e1, ~elev2, ~nvolumes_gr35_e2, ~vcp, ~percent_rain,
      1L, 9.1082, 5.4677, NA, 36.8916, 29911L, 7.2845, FALSE, 3.459612, 50L, 4.54545454545455, -0.3078956, NA, -6404.5974442746, NA, 851.2421, 12.5202, 0.3646, 216L, 1.3329, 0L, 852.3826,
      2L, 14.4428, 10.1568, NA, 44.6877, 49823L, 10.2681, FALSE, 8.230824, 150L, 13.6363636363636, -0.3078956, NA, -241.9400975439, NA, 212.6159, 11.5492, 0.3646, 216L, 1.3329, 0L, 213.7921
    )
  )
})
