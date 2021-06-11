test_that("download_vpfiles() returns error on incorrect parameters", {

  # Define default for testing
  date_min <- "2016-10-01"
  date_max <- "2016-11-30"
  radars <- c("bejab", "bewid")
  directory <- "~/bioRad_tmp_files"
  overwrite <- TRUE
  dir.create(directory)

  expect_error(download_vpfiles('01/01/2016', date_max, radars, directory, overwrite), "Incorrect date format: 01/01/2016", fixed = TRUE)
  expect_error(download_vpfiles(12345, date_max, radars, directory, overwrite), "date_min is not a string (a length one character vector).", fixed = TRUE)
  expect_error(download_vpfiles(c(date_min,date_max), date_max, radars, directory, overwrite), "date_min is not a string (a length one character vector).", fixed = TRUE)

  expect_error(download_vpfiles(date_min, '01/01/2016', radars, directory, overwrite), glue("Incorrect date format: ", '01/01/2016'), fixed = TRUE)
  expect_error(download_vpfiles(date_min ,12345, radars, directory, overwrite), "date_max is not a string (a length one character vector).", fixed = TRUE)
  expect_error(download_vpfiles(date_min, c(date_min,date_max), radars, directory, overwrite), "date_max is not a string (a length one character vector).", fixed = TRUE)

  expect_error(download_vpfiles(date_min, date_max, 'not_radar_code', directory, overwrite), "Radar codes should be 5 characters: not_radar_code", fixed = TRUE)
  expect_error(download_vpfiles(date_min, date_max, c("not_radar_code", "begwid"), directory, overwrite), "Radar codes should be 5 characters: not_radar_code", fixed = TRUE)
  expect_error(download_vpfiles(date_min, date_max, "abcde", directory, overwrite), "Radar codes don't exist: abcde", fixed = TRUE)


  expect_error(download_vpfiles(date_min, date_max, radars, 1, overwrite), "path is not a string (a length one character vector)", fixed = TRUE)
  expect_error(download_vpfiles(date_min, date_max, radars, 'not_a_directory', overwrite), "Path 'not_a_directory' does not exist", fixed = TRUE)

  expect_error(download_vpfiles(date_min, date_max, radars, directory, 'not_a_logical'), "overwrite is not a logical", fixed = TRUE)
})
