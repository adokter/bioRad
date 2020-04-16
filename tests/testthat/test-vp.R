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

test_that("By default, suntime is TRUE and suntime information is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp)

  # three colulmns are present:
  expect_is(df$sunset, "POSIXct")
  expect_is(df$sunrise, "POSIXct")
  expect_is(df$day, "logical")
})

test_that("If suntime is (explicitly) TRUE, suntime information is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp, suntime = TRUE)

  # three colulmns are present:
  expect_is(df$sunset, "POSIXct")
  expect_is(df$sunrise, "POSIXct")
  expect_is(df$day, "logical")
})

test_that("If suntime is FALSE, suntime information is *not* added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp, suntime = FALSE)

  # three colulmns are present:
  expect_null(df$sunset)
  expect_null(df$sunrise)
  expect_null(df$day)
})

test_that("By default, geo is TRUE and lat/lon/antenna height is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp)

  # three colulmns are present:
  expect_is(df$lat, "numeric")
  expect_is(df$lon, "numeric")
  expect_is(df$height_antenna, "numeric")
})

test_that("If geo is (explicitly) TRUE, lat/lon/antenna height is added to each row", {
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

test_that("If geo is FALSE, lat/lon/antenna height is added to each row", {
  data("example_vp")

  df <- as.data.frame(example_vp, geo = FALSE)

  # three colulmns are present:
  expect_null(df$lat)
  expect_null(df$lon)
  expect_null(df$height_antenna)
})

# TODO: test explicit row names (with row.names)
# TODO: test explicitly selecting quantities (also requensting a non-existent quantity)
# TODO: test "optional" argument
# TODO: test explicit lat/lon/elev (values overriden in lat/lon, but also correct sunrise/sunset calculation)
# TODO: replace "stopifnot" in vp.R?? Or is that out of scope
