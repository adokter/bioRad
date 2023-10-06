# Define default for testing
date_min <- as.POSIXct("2016-10-02 20:00", tz = "UTC")
date_max <- as.POSIXct("2016-10-02 20:05", tz = "UTC")
radars <- "KBBX"
directory <- tempdir()
overwrite <- TRUE

test_that("date input for download_pvolfiles() ", {
  skip_if_offline()
  # working with default
  expect_no_error(
    suppressMessages(
      download_pvolfiles(date_min, date_max, radars, directory, overwrite)
      )
  )

  # Working over multiple days
  expect_no_error(
    suppressMessages(
      download_pvolfiles(
        as.POSIXct("2016-10-02 23:55", tz = "UTC"),
        as.POSIXct("2016-10-03 00:07", tz = "UTC"),
        radars,
        directory,
        overwrite
      )
    )
  )

  # Wrong format
  expect_error(
    download_pvolfiles("01/01/2016", date_max, radars, directory, overwrite),
    "date_min is not a date",
    fixed = TRUE
  )
  expect_error(
    download_pvolfiles(12345, date_max, radars, directory, overwrite),
    "date_min is not a date",
    fixed = TRUE
  )
  expect_error(
    download_pvolfiles(c(date_min, date_max),
                       date_max,
                       radars,
                       directory,
                       overwrite),
    regexp = "Only one `date_min` should be provided.",
    fixed = TRUE
  )
  expect_error(
    download_pvolfiles(date_min,
                       date_max = c(date_min, date_max),
                       radars,
                       directory,
                       overwrite),
    regexp = "Only one `date_max` should be provided.",
    fixed = TRUE
  )
  expect_error(
    download_pvolfiles(
      date_min = date_max,
      date_max = date_min,
      radars, directory, overwrite
    ),
    regexp = "date_max is not greater or equal to date_min",
    fixed = TRUE
  )
  expect_error(
    download_pvolfiles(date_min, "01/01/2016", radars, directory, overwrite),
    regexp = "date_max is not a date",
    fixed = TRUE
  )
  expect_error(
    download_pvolfiles(date_min, 12345, radars, directory, overwrite),
    regexp = "date_max is not a date",
    fixed = TRUE
  )

  # Error on impossible date
  expect_error(
    download_pvolfiles(
      as.POSIXct("2016-10-02 20:05", tz = "UTC"),
      as.POSIXct("2016-10-02 20:00", tz = "UTC"),
      radars,
      directory,
      overwrite
    ),
    regexp = "date_max is not greater or equal to date_min",
    fixed = TRUE
  )
  expect_warning(
    suppressMessages(
      download_pvolfiles(
        as.POSIXct("2046-10-02 20:00", tz = "UTC"),
        as.POSIXct("2046-10-02 20:05", tz = "UTC"),
        radars,
        directory,
        overwrite
      )
    ),
    regexp = paste0(
      "No data availble on the 2046-10-02. ",
      "Please check data availability for this date."
    ),
    fixed = TRUE
  )
})

test_that("Check radar code for download_pvolfiles() ", {
  skip_if_offline()
  expect_warning(
    download_pvolfiles(date_min,
                       date_max,
                       c("KBBX", "KGHC"),
                       directory,
                       overwrite),
    "radar is not of length 1",
    fixed = TRUE
  )
  expect_warning(
    suppressMessages(
      download_pvolfiles(date_min, date_max, "ABCD", directory, overwrite)
    ),
    regexp = paste0(
      "No data available for ABCD on the 2016-10-02. Check radar",
      " code and data availability on",
      " https://noaa-nexrad-level2.s3.amazonaws.com/index.html"
    ),
    fixed = TRUE
  )
})

test_that("Check path and overwrite for download_pvolfiles() ", {
  skip_if_offline()
  expect_error(
    download_pvolfiles(date_min, date_max, radars, 1, overwrite),
    "path is not a string (a length one character vector)",
    fixed = TRUE
  )
  expect_error(
    download_pvolfiles(
      date_min,
      date_max,
      radars,
      "not_a_directory",
      overwrite
    ),
    regexp = "Path 'not_a_directory' does not exist",
    fixed = TRUE
  )
  expect_error(
    download_pvolfiles(date_min, date_max, radars, directory, "not_a_logical"),
    "overwrite is not a logical",
    fixed = TRUE
  )
})
