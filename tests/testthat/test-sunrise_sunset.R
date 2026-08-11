test_that("sunrise() returns error on incorrect parameters", {

})

test_that("sunset() returns error on incorrect parameters", {

})

test_that("sunrise in the Netherlands", {
  expect_s3_class(
    sunrise("2016-01-01", 5, 53),
    c("POSIXct", "POSIXt")
  )
  expect_true(sunrise("2016-01-01", 5, 53) <
    as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
})

test_that("sunset in the Netherlands", {
  expect_s3_class(
    sunset("2016-01-01", 5, 53),
    c("POSIXct", "POSIXt")
  )
  expect_true(sunset("2016-01-01", 5, 53) >
    as.POSIXct("2016-01-01 13:00:00 UTC", tz = "UTC"))
})

test_that("sf solar calculations preserve SpatialPoints results", {
  date <- as.POSIXct("2016-01-01", tz = "UTC")
  locations <- sp::SpatialPoints(
    cbind(lon = c(5, -76.5), lat = c(53, 42.4)),
    proj4string = sp::CRS("+proj=longlat +datum=WGS84")
  )
  legacy_sunrise <- suntools::crepuscule(
    locations, date, solarDep = 0.268,
    direction = "dawn", POSIXct.out = TRUE
  )$time
  legacy_sunset <- suntools::crepuscule(
    locations, date, solarDep = 0.268,
    direction = "dusk", POSIXct.out = TRUE
  )$time

  expect_equal(sunrise(date, c(5, -76.5), c(53, 42.4)), legacy_sunrise)
  expect_equal(sunset(date, c(5, -76.5), c(53, 42.4)), legacy_sunset)
})

test_that("sf solar calculations preserve time zones", {
  date <- as.POSIXct("2016-06-05", tz = "America/Chicago")
  datetime <- as.POSIXct(date, tz = "UTC")
  locations <- sp::SpatialPoints(
    cbind(lon = -98, lat = 45),
    proj4string = sp::CRS("+proj=longlat +datum=WGS84")
  )
  expected <- suntools::crepuscule(
    locations, datetime, solarDep = 0.268,
    direction = "dusk", POSIXct.out = TRUE
  )$time

  expect_equal(sunset(date, -98, 45), expected)
  expect_equal(
    sunset(date, -98, 45, force_tz = TRUE),
    lubridate::as_datetime(expected, tz = "UTC")
  )
})
