test_that("download_vpts_aloft() returns error on incorrect parameters", {
  # Define defaults for testing
  date_min <- "2016-10-01"
  date_max <- "2016-11-30"
  radars <- c("bejab", "bewid")
  directory <- tempdir()
  overwrite <- TRUE

  # Check source
  expect_error(
    download_vpts_aloft(directory,
      radars,
      start_date,
      end_date,
      source = "not a source!",
      format = "csv",
      overwrite
    ),
    regexp = "`source` must be one of: `baltrad`, `ecog-04003`",
    fixed = TRUE
  )

  # Check format
})
