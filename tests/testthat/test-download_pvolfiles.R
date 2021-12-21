test_that("download_pvolfiles() returns error on incorrect parameters", {

  # Define default for testing
  date_min <- "2016-10-02 20:00"
  date_max <- "2016-10-02 23:00"
  radars <- "KBBX"
  directory <- tempdir()
  overwrite <- TRUE

  expect_error(download_pvolfiles('01/01/2016', date_max, radars, directory, overwrite), "Incorrect date format: 01/01/2016", fixed = TRUE)
  expect_error(download_pvolfiles(12345, date_max, radars, directory, overwrite), "date_min is not a string (a length one character vector).", fixed = TRUE)
  expect_error(download_pvolfiles(c(date_min,date_max), date_max, radars, directory, overwrite), "date_min is not a string (a length one character vector).", fixed = TRUE)

  expect_error(download_pvolfiles(date_min, '01/01/2016', radars, directory, overwrite), "Incorrect date format: 01/01/2016", fixed = TRUE)
  expect_error(download_pvolfiles(date_min ,12345, radars, directory, overwrite), "date_max is not a string (a length one character vector).", fixed = TRUE)
  expect_error(download_pvolfiles(date_min, c(date_min,date_max), radars, directory, overwrite), "date_max is not a string (a length one character vector).", fixed = TRUE)

  expect_error(download_pvolfiles(date_min, date_max, c("KBBX", "KGHC"), directory, overwrite), "radar is not of length 1", fixed = TRUE)
  expect_error(download_pvolfiles(date_min, date_max, "ABCD", directory, overwrite), "radar ABCD doesn't exist", fixed = TRUE)

  expect_error(download_pvolfiles(date_min, date_max, radars, 1, overwrite), "path is not a string (a length one character vector)", fixed = TRUE)
  expect_error(download_pvolfiles(date_min, date_max, radars, 'not_a_directory', overwrite), "Path 'not_a_directory' does not exist", fixed = TRUE)

  expect_error(download_pvolfiles(date_min, date_max, radars, directory, 'not_a_logical'), "overwrite is not a logical", fixed = TRUE)
})
