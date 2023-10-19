test_that("as.vpts() returns error message for incorrect data", {
  df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))

  #randomly remove row
  randomIndex <- sample(nrow(df), 1)
  df <- df[-randomIndex, ]

  expect_error(as.vpts(df),"identical")
})


test_that("as.vpts() names reflectivity 'DBZH' or 'dbz_all' based on from_csv argument", {

  file <- system.file("extdata", "example_vpts.csv", package = "bioRad")

  # When as.vpts() is called via read_vpts(), the reflectivity variable is named dbz_all in the resulting data.frame
  vpts_df <-  as.data.frame(read_vpts(file))
  expect_true(!"DBZH" %in% colnames(vpts_df))
  expect_true("dbz_all" %in% colnames(vpts_df))

  # When as.vpts() is called on a dataframe, the reflectivity variable will be renamed DBZH in the resulting vpts object
  vpts_obj <- as.vpts(vpts_df, from_csv = FALSE)
  expect_true("DBZH" %in% names(vpts_obj$data))
  expect_true(!"dbz_all" %in% names(vpts_obj$data))

})
