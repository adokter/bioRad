vp <- example_vp

test_that("as.data.frame() returns error on incorrect parameters", {
  expect_error(as.data.frame(vp, row.names = "not_a_vector"))
  expect_error(as.data.frame(vp, suntime = "not_a_logical"))
  expect_error(as.data.frame(vp, geo = "not_a_logical"))
  expect_error(as.data.frame(vp, elev = "not_a_double"))
  expect_error(as.data.frame(vp, lat = "not_a_double"))
  expect_error(as.data.frame(vp, lon = "not_a_double"))
})

test_that("as.data.frame() returns a data frame", {
  expect_is(as.data.frame(vp), "data.frame")
})

test_that("as.data.frame() returns correct number of rows/cols", {
  df <- as.data.frame(vp)
  expect_equal(nrow(df), 25) # 25 rows in nrow(vp$data)
  expect_gte(ncol(df), 16) # 16 cols in ncol(vp$data), but extra added by bioRad
})

test_that("as.data.frame() returns the expected column names", {
  expected_col_names <- c(
    "radar", "datetime", "ff", "dbz", "dens", "u", "v", "gap", "w", "n_dbz",
    "dd", "n", "DBZH", "height", "n_dbz_all", "eta", "sd_vvp", "n_all", "lat",
    "lon", "height_antenna", "day", "sunrise", "sunset"
  )
  expect_equal(names(as.data.frame(vp)), expected_col_names)
})

test_that("as.data.frame() returns the correct data", {
  df <- as.data.frame(example_vp)

  expect_equal(unique(df[["radar"]]), "seang") # One unique radar
  expect_equal(as.character(unique(df[["datetime"]])), "2015-10-18 18:00:00") # One unique timestamp

  # Check a few randomly selected values (coming directly from the source data):
  expect_equal(df$height[1], 0)
  expect_equal(df$height[8], 1400)
  expect_equal(df$ff[4], 13.77482, tolerance = 0.001)
  expect_equal(df$dens[10], 17.3985100, tolerance = 0.001)
  expect_equal(is.na(df$sd_vvp[1]), TRUE)
  expect_equal(df$sd_vvp[13], 2.994742, tolerance = 0.001)
})

test_that("as.data.frame() includes lat/lon/height_antenna and can be assigned, unless geo = FALSE", {
  # lat/lon/height_antenna columns are added by default and taken from metadata
  df <- as.data.frame(example_vp)
  expect_equal(unique(df[["lat"]]), example_vp$attributes$where$lat)
  expect_equal(unique(df[["lon"]]), example_vp$attributes$where$lon)
  expect_equal(unique(df[["height_antenna"]]), example_vp$attributes$where$height)

  # lat/lon/height_antenna columns are missing if geo = FALSE
  df_geo_false <- as.data.frame(example_vp, geo = FALSE)
  expect_null(df_geo_false$lat)
  expect_null(df_geo_false$lon)
  expect_null(df_geo_false$height_antenna)

  # lat/lon can be set explicitly
  df_latlong <- as.data.frame(example_vp, lat = 50.6472, lon = 4.3603)
  expect_equal(unique(df_latlong[["lat"]]), 50.6472)
  expect_equal(unique(df_latlong[["lon"]]), 4.3603)
})


test_that("as.data.frame() includes sunset/sunrise/day cols, unless suntime = FALSE", {
    # sunset/sunrise/day columns are added by default
    df <- as.data.frame(example_vp)
    expect_is(df$sunset, "POSIXct")
    expect_is(df$sunrise, "POSIXct")
    expect_is(df$day, "logical")

    # sunset/sunrise/day columns are missing if suntime = FALSE
    df_suntime_false <- as.data.frame(example_vp, suntime = FALSE)
    expect_null(df_suntime_false$sunset)
    expect_null(df_suntime_false$sunrise)
    expect_null(df_suntime_false$day)
  }
)

test_that("as.data.frame() values in suntime/sunset/day cols are correct and updated with lat/lon", {
  # Note: timestamp for example_vp is 2015-10-18 18:00:00 UTC

  # 1. Check for lat/long contained in example_vp: 56.3675, 12.8517
  df <- as.data.frame(example_vp)

  # Manual data check on: https://www.suncalc.org/#/56.3675,12.8517,12/2015.10.18/09:00/1/3
  expected_sunrise <- as.POSIXlt("2015-10-18 05:45:26", tz = "UTC") # 07:45:26 UTC+2
  expected_sunset <- as.POSIXlt("2015-10-18 16:01:13", tz = "UTC") # 18:01:13 UTC+2

  expect_equal(as.POSIXlt(df$sunrise[1]), expected_sunrise, tolerance = 5) # Tolerance: minutes
  expect_equal(as.POSIXlt(df$sunset[1]), expected_sunset, tolerance = 5)
  expect_false(df$day[1]) # At the 18:00:00 UTC timestamp, it is night

  # 2. Set lat/lon to other values and check if it's still correct
  df <- as.data.frame(example_vp, lat = 50.6472, lon = 4.3603)

  # Manual data check on: https://www.suncalc.org/#/50.6472,4.3603,12/2015.10.18/09:00/1/3
  expected_sunrise <- as.POSIXlt("2015-10-18 06:09:09", tz = "UTC") # 08:09:09 UTC+2
  expected_sunset <- as.POSIXlt("2015-10-18 16:45:35", tz = "UTC") # 18:45:35 UTC+2

  expect_equal(as.POSIXlt(df$sunrise[1]), expected_sunrise, tolerance = 5)
  expect_equal(as.POSIXlt(df$sunset[1]), expected_sunset, tolerance = 5)
  expect_false(df$day[1]) # At the 18:00:00 UTC timestamp, it is night

  # 3. Let's go to Antartica, it should be day there
  df <- as.data.frame(example_vp, lat = -74.2486, lon = -1.2497)
  expect_true(df$day[1])
})

# TODO: test "row.names" parameter
# TODO: test "optional" parameter
# TODO: test "elev" parameter
