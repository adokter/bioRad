test_that("list_vpts_aloft() returns error for unknown source", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_error(
    list_vpts_aloft(
      date_min = "2000-01-01",
      date_max = "2001-12-15",
      radars = "itdec",
      source = "not a valid source"
    ),
    regexp = "`source` must be one of: `baltrad`, `ecog-04003`.",
    fixed = TRUE
  )
})

test_that("list_vpts_aloft() returns error for invalid format", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_error(
    list_vpts_aloft(
      date_min = "2000-01-01",
      date_max = "2001-12-15",
      radars = "itdec",
      format = "not a valid format"
    ),
    regexp = "`format` must be one of: `csv`, `hdf5`.",
    fixed = TRUE
  )
})

test_that("list_vpts_aloft() returns error if radar doesn't exist", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_error(
    list_vpts_aloft(
      date_min = "1990-01-01",
      date_max = "2050-01-01",
      radars = c("not a valid radar")
    ),
    regexp = "Can't find radar(s): not a valid radar",
    fixed = TRUE
  )
})

test_that("list_vpts_aloft() returns a character vector", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_type(
    list_vpts_aloft(
      date_min = "2023-02-01",
      date_max = "2023-05-01",
      radars = c("bejab", "bewid")
    ),
    "character"
  )
})

test_that("list_vpts_aloft() returns no warning when all dates are specified", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_no_warning(
    list_vpts_aloft(
      radars = "bejab",
      date_min = "2023-02-01",
      date_max = "2023-04-01"
    )
  )
})

test_that("list_vpts_aloft() works without specifying dates", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  # just date_min
  expect_no_error(
    suppressWarnings(list_vpts_aloft(
      date_min = "1900-01-01",
      radars = "frmtc"
    ))
  )
  # just date_max
  expect_no_error(
    suppressWarnings(list_vpts_aloft(
      date_max = Sys.Date(),
      radars = "bejab"
    ))
  )
  # neither provided
  expect_no_error(
    suppressWarnings(list_vpts_aloft(
      radars = "essse"
    ))
  )
})

test_that("list_vpts_aloft() returns all data when no dates are provided", {
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_gt(
    length(
      suppressWarnings(list_vpts_aloft(
        radars = "bejab"
      ))
    ),
    length(
      list_vpts_aloft(
        radars = "bejab",
        date_min = "2023-02-01",
        date_max = "2023-04-01"
      )
    )
  )
})


test_that("list_vpts_aloft() warns if data was found for subset of radars or if not all dates were found", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_warning(
    list_vpts_aloft(
      date_min = "1900-01-01",
      date_max = "2010-01-01",
      radars = c("plpas")
    ),
    regexp = "No data found for radar(s) between 1900-01-01 - 2010-01-01",
    fixed = TRUE
  )
  expect_warning(
    list_vpts_aloft(
      date_min = "2020-01-01",
      date_max = "2020-06-01",
      radars = c("nobml")
    ),
    "Radar data found between 2020-02 and 2020-06 but not every date has radar data")
})

test_that("list_vpts_aloft() warns and returns emtpy vector on no data found",{
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_equal(
    list_vpts_aloft(
      date_min = "1800-01-01",
      date_max = "1800-02-01",
      radars = "rssje",
      show_warnings = FALSE
    ),
    glue::glue()
  )
  expect_warning(
    list_vpts_aloft(
      date_min = "1800-01-01",
      date_max = "1800-02-01",
      radars = "rssje",
      show_warnings = TRUE
    )
  )
})

test_that("list_vpts_aloft() silences warnings with show_warnings argument", {
  withr::local_options(lifecycle_verbosity = "quiet")
  skip_if_no_aws.s3()
  skip_if_offline()
  expect_no_warning(
    list_vpts_aloft(
      date_min = "1900-01-01",
      date_max = "2023-05-22",
      radars = c("nobml", "plpas"),
      show_warnings = FALSE
    )
  )
  expect_no_warning(
    list_vpts_aloft(
      date_min = "1900-01-01",
      date_max = "2023-05-22",
      radars = "nobml",
      show_warnings = FALSE
    )
  )
})
