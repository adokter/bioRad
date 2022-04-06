# Define default for testing
date_min <- as.POSIXct("2016-10-02 20:00", tz = "UTC")
date_max <- as.POSIXct("2016-10-02 20:05", tz = "UTC")
radars <- "KBBX"
directory <- tempdir()
overwrite <- TRUE

test_that("date input for download_pvolfiles() ", {

  # working with default
  expect_error(download_pvolfiles(date_min, date_max, radars, directory, overwrite), NA, fixed = TRUE)

  # Working over multiple days
  expect_error(download_pvolfiles(as.POSIXct("2016-10-02 23:55", tz = "UTC"), as.POSIXct("2016-10-03 00:07", tz = "UTC"), radars, directory, overwrite), NA, fixed = TRUE)

  # Wrong format
  expect_error(download_pvolfiles("01/01/2016", date_max, radars, directory, overwrite), "date_min is not a date", fixed = TRUE)
  expect_error(download_pvolfiles(12345, date_max, radars, directory, overwrite), "date_min is not a date", fixed = TRUE)
  expect_error(download_pvolfiles(c(date_min, date_max), date_max, radars, directory, overwrite))
  expect_error(download_pvolfiles(date_min, "01/01/2016", radars, directory, overwrite), fixed = TRUE)
  expect_error(download_pvolfiles(date_min, 12345, radars, directory, overwrite), fixed = TRUE)
  expect_error(download_pvolfiles(date_min, c(date_min, date_max), radars, directory, overwrite), fixed = TRUE)

  # Error on impossible date
  expect_error(download_pvolfiles(as.POSIXct("2016-10-02 20:05", tz = "UTC"), as.POSIXct("2016-10-02 20:00", tz = "UTC"), radars, directory, overwrite), fixed = TRUE)
  expect_error(download_pvolfiles(as.POSIXct("2046-10-02 20:00", tz = "UTC"), as.POSIXct("2046-10-02 20:05", tz = "UTC"), radars, directory, overwrite), fixed = TRUE)
})

test_that("Check radar code for download_pvolfiles() ", {
  expect_error(download_pvolfiles(date_min, date_max, c("KBBX", "KGHC"), directory, overwrite), "radar is not of length 1", fixed = TRUE)
  expect_error(download_pvolfiles(date_min, date_max, "ABCD", directory, overwrite), fixed = TRUE)
})

test_that("Check path and overwrite for download_pvolfiles() ", {
  expect_error(download_pvolfiles(date_min, date_max, radars, 1, overwrite), "path is not a string (a length one character vector)", fixed = TRUE)
  expect_error(download_pvolfiles(date_min, date_max, radars, "not_a_directory", overwrite), "Path 'not_a_directory' does not exist", fixed = TRUE)

  expect_error(download_pvolfiles(date_min, date_max, radars, directory, "not_a_logical"), "overwrite is not a logical", fixed = TRUE)
})
