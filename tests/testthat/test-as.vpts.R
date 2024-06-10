test_that("as.vpts() returns error message for incorrect data", {
  df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))

  #randomly remove row
  randomIndex <- sample(nrow(df), 1)
  df <- df[-randomIndex, ]

  expect_error(as.vpts(df),"identical")
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

# Test that the function issues a correct warning for multiple radar_longitude values
test_that("Warning is issued for multiple radar_longitude values", {
  file <- system.file("extdata", "example_vpts.csv", package = "bioRad")
  vpts_df <-  read_vpts(file, data_frame=TRUE)
  vpts_df$radar_longitude[1] <- vpts_df$radar_longitude[1] + 0.1
  expect_warning(
    modified_df <- as.vpts(vpts_df),
    "multiple `radar_longitude` values found"
  )
})

# Test that the function sets all radar_longitude values to the first one if it's a multi-value attribute
test_that("values are set to the first for multi-value attributes", {
  file <- system.file("extdata", "example_vpts.csv", package = "bioRad")
  vpts_df <-  read_vpts(file, data_frame=TRUE)
  vpts_df$radar_longitude[1] <- vpts_df$radar_longitude[1] + 0.1
  expect_warning(
    vpts_obj <- as.vpts(vpts_df),
    "multiple `radar_longitude` values found"
  )
  expect_equal(vpts_obj$attributes$where$lon, vpts_df$radar_longitude[1])

})
