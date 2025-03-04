test_that("as.vpts() returns warning message for incorrect data", {
  df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))

  #remove top bin of the third profile, creating a profile with lower max height
  df <- df[-which(df$height==max(df$height))[3], ]
  expect_warning(expect_warning(as.vpts(df),"Profiles found with different"),"Profiles found with different number of height layers")

  #randomly remove row
  randomIndex <- sample(nrow(df), 1)
  df <- df[-randomIndex, ]

  expect_warning(expect_warning(expect_warning(as.vpts(df),"Profiles found with different"),
                 "Profiles found with different altitude interval"),
                 "Profiles found with different number of height layers")

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

# Test that duplicate profiles are identified
test_that("profiles with identical timestamps are identified", {
  file <- system.file("extdata", "example_vpts.csv", package = "bioRad")
  vpts_df <-  read_vpts(file, data_frame=TRUE)
  vpts_duplicate <- as.vpts(rbind(vpts_df,vpts_df))
  vpts_single <- as.vpts(vpts_df)
  # vpts with duplicates should contain twice as many profiles:
  expect_equal(dim(vpts_duplicate)[1], 2*dim(vpts_single)[1])
  # duplicate profiles should be identical:
  vp_df=as.data.frame(example_vp, suntime=F)
  expect_identical(as.vpts(rbind(vp_df,vp_df))[1],as.vpts(rbind(vp_df,vp_df))[2])
})




