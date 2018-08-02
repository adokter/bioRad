context("test-sunrise_sunset.R")

test_that("sunrise in the Netherlands", {
  # deprecated sunrise-suntime function
  expect_is(suntime("2016-01-01", 5, 53),
            c("POSIXct", "POSIXt"))
  expect_true(suntime("2016-01-01", 5, 53) <
                as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
  # new function sunrise
  expect_is(sunrise("2016-01-01", 5, 53),
            c("POSIXct", "POSIXt"))
  expect_true(sunrise("2016-01-01", 5, 53) <
                as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
})

test_that("sunset in the Netherlands", {
  # deprecated sunrise-suntime function
  expect_is(suntime("2016-01-01", 5, 53, rise = FALSE),
            c("POSIXct", "POSIXt"))
  expect_true(suntime("2016-01-01", 5, 53, rise = FALSE) >
               as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
  # new function sunset
  expect_is(sunset("2016-01-01", 5, 53),
            c("POSIXct", "POSIXt"))
  expect_true(sunset("2016-01-01", 5, 53) >
                as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))


})
