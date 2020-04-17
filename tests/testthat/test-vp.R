context("test-vp.R")

test_that("as.data.frame() returns a data frame", {
  data("example_vp")

  expect_is(as.data.frame(example_vp), "data.frame")
})

test_that("as.data.frame() returns the correct number of rows / cols", {
  data("example_vp")

  df <- as.data.frame(example_vp)

  expect_equal(nrow(df), 25) # 25 rows in the test data
  expect_gt(ncol(df), 16)  # 16 cols in the test data, but bioRad will add a few
})

test_that("as.data.frame() includes the (correct) radar name", {
  data("example_vp")

  df <- as.data.frame(example_vp)
  expect_equal(unique(df[["radar"]]), "seang") # All rows have the same "seang" value
})

test_that("as.data.frame() includes the (correct) datetime", {
  data("example_vp")

  df <- as.data.frame(example_vp)
  expect_equal(as.character(unique(df[["datetime"]])), "2015-10-18 18:00:00") # All rows have the same "seang" value
})

test_that("as.data.frame() returns the correct data", {
  data("example_vp")

  df <- as.data.frame(example_vp)

  # Check a few randomly selected values (coming directly from the source data):
  expect_equal(df$height[1], 0)
  expect_equal(df$height[8], 1400)
  expect_equal(df$ff[4], 13.77482, tolerance = 0.001)
  expect_equal(df$dens[10], 17.3985100, tolerance = 0.001)
  expect_equal(is.na(df$sd_vvp[1]), TRUE)
  expect_equal(df$sd_vvp[13], 2.994742, tolerance = 0.001)
})

test_that("as.data.frame(): by default, suntime is TRUE and suntime information is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp)

  # three colulmns are present:
  expect_is(df$sunset, "POSIXct")
  expect_is(df$sunrise, "POSIXct")
  expect_is(df$day, "logical")
})

test_that("as.data.frame(): if suntime is (explicitly) TRUE, suntime information is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp, suntime = TRUE)

  # three colulmns are present:
  expect_is(df$sunset, "POSIXct")
  expect_is(df$sunrise, "POSIXct")
  expect_is(df$day, "logical")
})

test_that("as.data.frame(): values in suntime/sunset/day cols are correct", {
  data("example_vp")

  df <- as.data.frame(example_vp, suntime = TRUE)

  # 1. With lat/lon coming from example_vp: 56.3675, 12.8517 (date: 2015-10-18)
  # Manual data check on: https://www.suncalc.org/#/56.3675,12.8517,12/2015.10.18/09:06/1/3
  # According to the website above, sunrise is at 7:45:26 (UTC+2) and sunset at 18:01:13 (UTC+2)
  expected_sunrise <- with_tz(as.POSIXlt("2015-10-18 07:45:26", tz="Europe/Stockholm"), 'UTC')
  expected_sunset <- with_tz(as.POSIXlt("2015-10-18 18:01:13", tz="Europe/Stockholm"), 'UTC')

  expect_equal(as.POSIXlt(df$sunrise[1]), expected_sunrise, tolerance = 5) # Apparently, tolerance is expressed in minutes
  expect_equal(as.POSIXlt(df$sunset[1]), expected_sunset, tolerance = 5)
  expect_false(df$day[1]) # it should therefore be night at 18:00 local time

  # 2. We force lat/lon to other values and check if it's still correct
  df <- as.data.frame(example_vp, lat = 50.6472, lon = 4.3603)
  expected_sunrise <- with_tz(as.POSIXlt("2015-10-18 08:09:09", tz="Europe/Brussels"), 'UTC')
  expected_sunset <- with_tz(as.POSIXlt("2015-10-18 18:45:35", tz="Europe/Brussels"), 'UTC')
  expect_equal(as.POSIXlt(df$sunrise[1]), expected_sunrise, tolerance = 5) # Apparently, tolerance is expressed in minutes
  expect_equal(as.POSIXlt(df$sunset[1]), expected_sunset, tolerance = 5)
  expect_false(df$day[1])

  # 3. Let's go to Antartica, it should be day there
  df <- as.data.frame(example_vp, lat = -74.2486, lon = -1.2497)
  expect_true(df$day[1])
})

test_that("as.data.frame(): if suntime is FALSE, suntime information is *not* added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp, suntime = FALSE)

  # three colulmns are present:
  expect_null(df$sunset)
  expect_null(df$sunrise)
  expect_null(df$day)
})

test_that("as.data.frame(): by default, geo is TRUE and lat/lon/antenna height is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp)

  # three colulmns are present:
  expect_is(df$lat, "numeric")
  expect_is(df$lon, "numeric")
  expect_is(df$height_antenna, "numeric")
})

test_that("as.data.frame(): if geo is (explicitly) TRUE, lat/lon/antenna height is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp, geo = TRUE)

  # three colulmns are present:
  expect_is(df$lat, "numeric")
  expect_is(df$lon, "numeric")
  expect_is(df$height_antenna, "numeric")

  lat_from_metadata <- example_vp$attributes$where$lat
  lon_from_metadata <- example_vp$attributes$where$lon
  height_from_metadata <- example_vp$attributes$where$height

  # Let's also check the values are correct (and identical on every row):
  expect_equal(unique(df[["lat"]]), lat_from_metadata)
  expect_equal(unique(df[["lon"]]), lon_from_metadata)
  expect_equal(unique(df[["height_antenna"]]), height_from_metadata)
})

test_that("as.data.frame(): if geo is FALSE, lat/lon/antenna height is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp, geo = FALSE)

  # three colulmns are present:
  expect_null(df$lat)
  expect_null(df$lon)
  expect_null(df$height_antenna)
})

test_that("as.data.frame(): if we manually set lat/lon, those are the value that should appear in the df", {
  data("example_vp")

  df <- as.data.frame(example_vp, geo = TRUE, lat = 50.6472, lon = 4.3603)

  # three colulmns are present:
  expect_equal(unique(df[["lat"]]), 50.6472)
  expect_equal(unique(df[["lon"]]), 4.3603)
})

test_that("as.data.frame(): users can manually choose included quantities", {
  data("example_vp")

  df <- as.data.frame(example_vp, quantities = c("dens"))
  expect_is(df$dens, "numeric")

  # FIXME: If we only request "dens", shouldn't "dd" be missing?
  # expect_null(df$dd)

  # We get a proper error message if requesting a nonexistent quantity
  expected_message <- "azertyuiop not an available quantity, select one or more of ff,dbz,dens,u,v,gap,w,n_dbz,dd,n,DBZH,height,n_dbz_all,eta,sd_vvp,n_all"
  expect_error(as.data.frame(example_vp, quantities = c("azertyuiop")), expected_message)
})

# TODO: test explicit row names (with row.names)
# TODO: test "optional" argument

