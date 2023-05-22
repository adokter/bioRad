test_that("list_vpts_aloft() returns error for unknown source", {
  expect_error(
    list_vpts_aloft(date_min = "2000-01-01",
                    date_max = "2001-12-15",
                    radars = "itdec",
                    source = "not a valid source"),
    regexp = "`source` must be one of: `baltrad`, `ecog-04003`.",
    fixed = TRUE
    )
})

test_that("list_vpts_aloft() returns error for invalid format", {
  expect_error(
    list_vpts_aloft(date_min = "2000-01-01",
                    date_max = "2001-12-15",
                    radars = "itdec",
                    format = "not a valid format"),
    regexp = "`format` must be one of: `csv`, `hdf5`.",
    fixed = TRUE
  )
})

test_that("list_vpts_aloft() returns error if radar doesn't exist", {
  expect_error(list_vpts_aloft(date_min = "1990-01-01",
                  date_max = "2050-01-01",
                  radars = c("not a valid radar")),
               regexp = "Can't find radar(s): not a valid radar",
               fixed = TRUE)
})

test_that("list_vpts_aloft() returns a character vector", {
  expect_type(
    list_vpts_aloft(date_min = "1990-01-01",
                    date_max = "2050-01-01",
                    radars = c("bejab","bewid")),
    "character"
  )
})


test_that("list_vpts_aloft() works without specifying dates", {
  # just date_min
  expect_no_error(
    list_vpts_aloft(
      date_min = "1900-01-01",
      radars = "frmtc"
    )
  )
  # just date_max
  expect_no_error(
    list_vpts_aloft(
      date_max = Sys.Date(),
      radars = "ukhhd"
    )
  )
  # neither provided
  expect_no_error(
    list_vpts_aloft(
      radars = "essse"
    )
  )

})

test_that("list_vpts_aloft() returns all data when no dates are provided", {

})


test_that("list_vpts_aloft() doesn't return the base url on missing data" ,{
  expect_false(
    list_vpts_aloft(date_min = "1900-01-01",
                    date_max = Sys.Date(),
                    radars = c("atval")) ==
      "https://aloft.s3-eu-west-1.amazonaws.com/")
})

test_that("list_vpts_aloft() warns if data was found subset of radars",{
  expect_warning(
    list_vpts_aloft(
      date_min = "1900-01-01",
      date_max = "2023-05-22",
      radars = c("nobml", "plpas")
    ),
    regexp = "Found no data for radars: `plpas`",
    fixed = TRUE
  )
})
