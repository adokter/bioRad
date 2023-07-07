vp <- example_vp
vpts <- example_vpts

# No tests for error on incorrect parameters:
# as.data.frame() is generic and work for every input
# TODO: test if "elev" parameter is passed correctly

test_that("as.data.frame().vp returns error on incorrect parameters", {
  expect_error(
    as.data.frame(vp, row.names = "not_a_logical"),
    "`row.names` must be a character vector of length"
  )
  expect_error(
    as.data.frame(vp, optional = "not_a_logical"),
    regexp = "optional is not a flag (a length one logical vector).",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vp, geo = "not_a_logical"),
    regexp = "geo is not a flag (a length one logical vector).",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vp, suntime = "not_a_logical"),
    regexp = "suntime is not a flag (a length one logical vector).",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vp, lat = "not_a_double"),
    regexp = "lat is not a numeric or integer vector",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vp, lon = "not_a_double"),
    regexp = "lon is not a numeric or integer vector",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vp, elev = "not_a_double"),
    regexp = "elev is not a numeric or integer vector",
    fixed = TRUE
  )

  expect_error(as.data.frame(vpts, row.names = "not_a_vector"),
    regexp = "`row.names` must be a character vector of length",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vpts, optional = "not_a_logical"),
    regexp = "optional is not a flag (a length one logical vector).",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vpts, geo = "not_a_logical"),
    regexp = "geo is not a flag (a length one logical vector).",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vpts, suntime = "not_a_logical"),
    regexp = "suntime is not a flag (a length one logical vector).",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vpts, lat = "not_a_double"),
    regexp = "lat is not a numeric or integer vector",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vpts, lon = "not_a_double"),
    regexp = "lon is not a numeric or integer vector",
    fixed = TRUE
  )
  expect_error(
    as.data.frame(vpts, elev = "not_a_double"),
    regexp = "elev is not a numeric or integer vector",
    fixed = TRUE
  )
})

test_that("as.data.frame() returns a data frame", {
  expect_s3_class(as.data.frame(vp), "data.frame")
  expect_s3_class(as.data.frame(vpts), "data.frame")
})

test_that("as.data.frame() returns correct number of rows/cols", {
  vp_df <- as.data.frame(vp, geo = FALSE, suntime = FALSE)
  expect_equal(nrow(vp_df), 25) # 25 rows in nrow(vp$data)
  expect_equal(ncol(vp_df), 18) # radar, datetime + 16 quantities

  vpts_df <- as.data.frame(vpts, geo = FALSE, suntime = FALSE)
  expect_equal(nrow(vpts_df), 25 * 1934) # 25 rows * 1934 datetimes
  expect_equal(ncol(vpts_df), 18) # radar, datetime + 16 quantities
})

test_that("as.data.frame() returns the expected column names", {
  expected_col_names <- c(
    "radar", "datetime", "ff", "dbz", "dens", "u", "v", "gap", "w", "n_dbz",
    "dd", "n", "DBZH", "height", "n_dbz_all", "eta", "sd_vvp", "n_all", "radar_latitude",
    "radar_longitude", "radar_height", "radar_wavelength", "day", "sunrise", "sunset"
  )
  expect_equal(names(as.data.frame(vp)), expected_col_names)
  # expect_equal(names(as.data.frame(vpts)), expected_col_names) # See #382
})

test_that("as.data.frame() returns the correct data", {
  vp_df <- as.data.frame(vp)
  expect_equal(unique(vp_df[["radar"]]), "seang") # One unique radar
  expect_equal(as.character(unique(vp_df[["datetime"]])), "2015-10-18 18:00:00") # One unique timestamp
  # Check a few randomly selected values (coming directly from the source data)
  expect_equal(vp_df$height[1], 0)
  expect_equal(vp_df$height[8], 1400)
  expect_equal(vp_df$ff[4], 13.77482, tolerance = 0.001)
  expect_equal(vp_df$dens[10], 17.3985100, tolerance = 0.001)
  expect_equal(is.na(vp_df$sd_vvp[1]), TRUE)
  expect_equal(vp_df$sd_vvp[13], 2.994742, tolerance = 0.001)

  vpts_df <- as.data.frame(vpts)
  expect_equal(unique(vpts_df[["radar"]]), "KBGM") # One unique radar
  expect_equal(length(unique(vpts_df[["datetime"]])), 1934) # 1934 unique timestamps
  # No check for randomly selected values for vpts
})

test_that("as.data.frame() allows to assign row.names", {
  vp_neg_rownames <- as.character(-1:-25) # c("-1", "-2", ..., "-25")
  vpts_neg_rownames <- as.character(-1:-48350) # c("-1", "-2", ..., "-48350")
  expect_equal(rownames(as.data.frame(vp, row.names = vp_neg_rownames)), vp_neg_rownames)
  expect_equal(rownames(as.data.frame(vpts, row.names = vpts_neg_rownames)), vpts_neg_rownames)
})

test_that("as.data.frame() includes lat/lon/height_antenna cols and can be assigned, unless geo = FALSE", {
  # lat/lon/height_antenna columns are added by default and taken from metadata
  vp_df <- as.data.frame(vp)
  expect_equal(unique(vp_df[["radar_latitude"]]), vp$attributes$where$lat)
  expect_equal(unique(vp_df[["radar_longitude"]]), vp$attributes$where$lon)
  expect_equal(unique(vp_df[["radar_height"]]), vp$attributes$where$height)

  vpts_df <- as.data.frame(vpts)
  expect_equal(unique(vpts_df[["radar_latitude"]]), vpts$attributes$where$lat)
  expect_equal(unique(vpts_df[["radar_longitude"]]), vpts$attributes$where$lon)
  expect_equal(unique(vpts_df[["radar_height"]]), vpts$attributes$where$height)

  # lat/lon/height_antenna columns are missing if geo = FALSE
  vp_df_geo_false <- as.data.frame(vp, geo = FALSE)
  expect_null(vp_df_geo_false$lat)
  expect_null(vp_df_geo_false$lon)
  expect_null(vp_df_geo_false$height_antenna)

  vpts_df_geo_false <- as.data.frame(vpts, geo = FALSE)
  expect_null(vpts_df_geo_false$lat)
  expect_null(vpts_df_geo_false$lon)
  expect_null(vpts_df_geo_false$height_antenna)

  # lat/lon can be set explicitly
  vp_df_latlong <- as.data.frame(vp, lat = 50.6, lon = 4.3)
  expect_equal(unique(vp_df_latlong[["radar_latitude"]]), 50.6)
  expect_equal(unique(vp_df_latlong[["radar_longitude"]]), 4.3)

  vpts_df_latlong <- as.data.frame(vpts, lat = 50.6, lon = 4.3)
  expect_equal(unique(vpts_df_latlong[["radar_latitude"]]), 50.6)
  expect_equal(unique(vpts_df_latlong[["radar_longitude"]]), 4.3)
})

test_that("as.data.frame() includes sunset/sunrise/day cols, unless suntime = FALSE", {
  # sunset/sunrise/day columns are added by default
  vp_df <- as.data.frame(vp)
  expect_s3_class(vp_df$sunset, "POSIXct")
  expect_s3_class(vp_df$sunrise, "POSIXct")
  expect_type(vp_df$day, "logical")

  vpts_df <- as.data.frame(vpts)
  expect_s3_class(vpts_df$sunset, "POSIXct")
  expect_s3_class(vpts_df$sunrise, "POSIXct")
  expect_type(vpts_df$day, "logical")

  # sunset/sunrise/day columns are missing if suntime = FALSE
  vp_df_suntime_false <- as.data.frame(vp, suntime = FALSE)
  expect_null(vp_df_suntime_false$sunset)
  expect_null(vp_df_suntime_false$sunrise)
  expect_null(vp_df_suntime_false$day)

  vpts_df_suntime_false <- as.data.frame(vpts, suntime = FALSE)
  expect_null(vpts_df_suntime_false$sunset)
  expect_null(vpts_df_suntime_false$sunrise)
  expect_null(vpts_df_suntime_false$day)
})

test_that("as.data.frame.vp() values in sunrise/sunset/day cols are correct and updated with lat/lon", {
  # Only tested for vp (not vpts), which has timestamp 2015-10-18 18:00:00 UTC

  # 1. Check for lat/long contained in vp: 56.3675, 12.8517
  vp_df <- as.data.frame(vp)

  # Manual data check on: https://www.suncalc.org/#/56.3675,12.8517,12/2015.10.18/09:00/1/3
  expected_sunrise <- as.POSIXlt("2015-10-18 05:45:26", tz = "UTC") # 07:45:26 UTC+2
  expected_sunset <- as.POSIXlt("2015-10-18 16:01:13", tz = "UTC") # 18:01:13 UTC+2
  ## Test that results are within 5 minutes of manual data check
  expect_lt(
    abs(lubridate::ymd_hms(vp_df$sunrise[1]) - lubridate::ymd_hms(expected_sunrise)),
    lubridate::dminutes(5)
  )
  expect_lt(
    abs(lubridate::ymd_hms(vp_df$sunset[1]) - lubridate::ymd_hms(expected_sunset)),
    lubridate::dminutes(5)
  )
  expect_false(vp_df$day[1]) # At the 18:00:00 UTC timestamp, it is night

  # 2. Set lat/lon to other values and check if it's still correct
  vp_df <- as.data.frame(vp, lat = 50.6472, lon = 4.3603)

  # Manual data check on: https://www.suncalc.org/#/50.6472,4.3603,12/2015.10.18/09:00/1/3
  expected_sunrise <- as.POSIXlt("2015-10-18 06:09:09", tz = "UTC") # 08:09:09 UTC+2
  expected_sunset <- as.POSIXlt("2015-10-18 16:45:35", tz = "UTC") # 18:45:35 UTC+2

  expect_equal(as.POSIXlt(vp_df$sunrise[1]), expected_sunrise, tolerance = 5)
  expect_equal(as.POSIXlt(vp_df$sunset[1]), expected_sunset, tolerance = 5)
  expect_false(vp_df$day[1]) # At the 18:00:00 UTC timestamp, it is night

  # 3. Let's go to Antartica, it should be day there
  vp_df <- as.data.frame(vp, lat = -74.2486, lon = -1.2497)
  expect_true(vp_df$day[1])
})
