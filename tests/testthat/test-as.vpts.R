test_that("as.vpts() returns error message for incorrect data", {
  df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))

  #randomly remove row
  randomIndex <- sample(nrow(df), 1)
  df <- df[-randomIndex, ]

  expect_error(as.vpts(df),"identical")
})

test_that("as.vpts() handles multiple unique attribute values correctly", {

  original_df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))

  if (nrow(original_df) > 1) {
    df <- original_df
    df$radar_longitude[2] <- df$radar_longitude[1] + 0.1  # Change longitude slightly
    expect_warning(as.vpts(df), "multiple `radar_longitude` values found, storing only first")
  }

  if (nrow(original_df) > 1) {
    df <- original_df
    df$radar_latitude[2] <- df$radar_latitude[1] + 0.1    # Change latitude slightly
    expect_warning(as.vpts(df), "multiple `radar_latitude` values found, storing only the first")
  }

  if (nrow(original_df) > 1) {
    df <- original_df
    df$rcs[2] <- df$rcs[1] * 1.1                          # Change rcs slightly
    expect_warning(as.vpts(df), "multiple `rcs` values found, storing only the first")
  }

  if (nrow(original_df) > 1) {
    df <- original_df
    df$sd_vvp_threshold[2] <- df$sd_vvp_threshold[1] + 0.1 # Change sd_vvp_threshold slightly
    expect_warning(as.vpts(df), "multiple `sd_vvp_threshold` values found, storing only the first")
  }
})

test_that("as.vpts() converts reflectivity `dbz_all` into 'DBZH'", {

  file <- system.file("extdata", "example_vpts.csv", package = "bioRad")

  # When as.vpts() is called via read_vpts(), the reflectivity variable is named dbz_all in the resulting data.frame
  vpts_df <-  read_vpts(file, data_frame=TRUE)
  expect_true(!"DBZH" %in% colnames(vpts_df))
  expect_true("dbz_all" %in% colnames(vpts_df))

  # When as.vpts() is called on a dataframe, the reflectivity variable will be renamed DBZH in the resulting vpts object
  vpts_obj <- as.vpts(vpts_df)
  expect_true("DBZH" %in% names(vpts_obj$data))
  expect_true(!"dbz_all" %in% names(vpts_obj$data))

})
