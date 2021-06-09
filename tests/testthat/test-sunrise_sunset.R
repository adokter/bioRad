test_that("sunrise() returns error on incorrect parameters", {

})

test_that("sunset() returns error on incorrect parameters", {

})

test_that("sunrise in the Netherlands", {
  expect_is(
    sunrise("2016-01-01", 5, 53),
    c("POSIXct", "POSIXt")
  )
  expect_true(sunrise("2016-01-01", 5, 53) <
    as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
})

test_that("sunset in the Netherlands", {
  expect_is(
    sunset("2016-01-01", 5, 53),
    c("POSIXct", "POSIXt")
  )
  expect_true(sunset("2016-01-01", 5, 53) >
    as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
})
