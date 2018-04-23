context("test-sunrise_sunset.R")

test_that("sunrise in the Netherlands", {
  # deprecated sunrise-suntime function
  expect_is(suntime(5, 53, "2016-01-01"),
            c("POSIXct", "POSIXt"))
  expect_true(suntime(5, 53, "2016-01-01") <
                as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
  # new function sunrise
  expect_is(sunrise(5, 53, "2016-01-01"),
            c("POSIXct", "POSIXt"))
  expect_true(sunrise(5, 53, "2016-01-01") <
                as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
})

test_that("sunset in the Netherlands", {
  # deprecated sunrise-suntime function
  expect_is(suntime(5, 53, "2016-01-01", rise = FALSE),
            c("POSIXct", "POSIXt"))
  expect_true(suntime(5, 53, "2016-01-01", rise = FALSE) >
               as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
  # new function sunset
  expect_is(sunset(5, 53, "2016-01-01"),
            c("POSIXct", "POSIXt"))
  expect_true(sunset(5, 53, "2016-01-01") >
                as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))


})
