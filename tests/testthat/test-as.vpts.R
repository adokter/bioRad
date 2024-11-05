test_that("as.vpts() returns warning message for incorrect data", {
  df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))

  #remove top bin of the third profile, creating a profile with lower max height
  df <- df[-which(df$height==max(df$height))[3], ]
  expect_warning(as.vpts(df),"profiles found with different")

  #randomly remove row
  randomIndex <- sample(nrow(df), 1)
  df <- df[-randomIndex, ]

  expect_warning(as.vpts(df),"profiles found with different")
})

test_that("as.vpts() handles multiple unique attribute values correctly", {

  original_df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))

  if (nrow(original_df) > 1) {
    df <- original_df
    df$radar_longitude[2] <- df$radar_longitude[1] + 0.1  # Change longitude slightly
    expected_warning <- "multiple radar_longitude values found"
    expect_warning(as.vpts(df), regexp=expected_warning)
  }

  if (nrow(original_df) > 1) {
    df <- original_df
    df$radar_latitude[2] <- df$radar_latitude[1] + 0.1  # Change longitude slightly
    expected_warning <- "multiple radar_latitude values found"
    expect_warning(as.vpts(df), regexp=expected_warning)
  }

  if (nrow(original_df) > 1) {
    df <- original_df
    df$rcs[2] <- df$rcs[1] * 1.1 # Change rcs slightly
    expected_warning <- "multiple rcs values found"
    expect_warning(as.vpts(df), regexp=expected_warning)
  }

  if (nrow(original_df) > 1) {
    df <- original_df
    df$sd_vvp_threshold[2] <- df$sd_vvp_threshold[1] + 0.1 # Change sd_vvp_threshold slightly
    expected_warning <-  "multiple sd_vvp_threshold values found"
    expect_warning(as.vpts(df), regexp=expected_warning)
  }
})

# Test that the function issues a correct warning for multiple radar_longitude values
test_that("Warning is issued for multiple radar_longitude values", {
  file <- system.file("extdata", "example_vpts.csv", package = "bioRad")
  vpts_df <-  read_vpts(file, data_frame=TRUE)
  vpts_df$radar_longitude[1] <- vpts_df$radar_longitude[1] + 0.1
  expect_warning(
    as.vpts(vpts_df),
    regexp="multiple radar_longitude values found"
  )
})

# Test that the function sets all radar_longitude values to the first one if it's a multi-value attribute
test_that("values are set to the first for multi-value attributes", {
  file <- system.file("extdata", "example_vpts.csv", package = "bioRad")
  vpts_df <-  read_vpts(file, data_frame=TRUE)
  vpts_df$radar_longitude[1] <- vpts_df$radar_longitude[1] + 0.1
  expect_warning(
    as.vpts(vpts_df),
    regexp="multiple radar_longitude values found"
  )
  #expect_equal(vpts_obj$attributes$where$lon, vpts_df$radar_longitude[1])

})
