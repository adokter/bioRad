vpts <- example_vpts
vp <- example_vp

# No tests for error on incorrect parameters:
# summary(), print(), dim() are generic and work for every input
# is.vpts() returns TRUE/FALSE and works for every input
# vpts["not_numeric] will return NA

test_that("summary.vpts() prints metadata to the console", {
  # print.vpts() is not tested as it is the same as and called from summary.vpts()
  expect_output(summary(vpts), "Irregular time series of vertical profiles (class vpts)", fixed = TRUE)
  expect_output(summary(vpts), "radar:", fixed = TRUE)
  expect_output(summary(vpts), "# profiles:", fixed = TRUE)
  expect_output(summary(vpts), "time range (UTC):", fixed = TRUE)
  expect_output(summary(vpts), "time step (s):", fixed = TRUE)

  regular_vpts <- regularize_vpts(vpts)
  expect_output(summary(regular_vpts), "Regular time series of vertical profiles (class vpts)", fixed = TRUE)
})

test_that("summary.vpts() warns for legacy objects", {
  names(vpts) <- sub("height", "heights", names(vpts)) # Rename to legacy "heights"
  expect_warning(summary(vpts), "`x` is a legacy vpts object without a column `height`.", fixed = TRUE)
  vpts <- convert_legacy(vpts) # Reset
  names(vpts) <- sub("datetime", "dates", names(vpts)) # Rename to legacy "dates"
  expect_warning(summary(vpts), "`x` is a legacy vpts object without a column `datetime`.", fixed = TRUE)
})

test_that("is.vpts() returns TRUE/FALSE correctly", {
  expect_true(is.vpts(vpts))
  expect_false(is.vpts("not_a_vpts"))
  expect_false(is.vpts(vp))
})

test_that("dim.vpts() returns number of datetimes, heights, quantities", {
  expect_vector(dim(vpts))
  expect_equal(dim(vpts), c(1934, 25, 15)) # 1934 datetimes, 25 heights, 15 quantities
})

test_that("[.vpts subsets by profiles", {
  # 1934 profiles in total
  expect_equal(length(vpts[10]$datetime), 1) # Select 10th => 1 profile
  expect_equal(length(vpts[10:20]$datetime), 11) # Select 10:20 => 11 profiles
  expect_equal(length(vpts[-1:-1900]$datetime), 34) # Remove 1:1900 => 34 profiles left
})

test_that("[.vpts returns a vp object for single selection", {
  expect_s3_class(vpts[10:20], "vpts")
  expect_s3_class(vpts[10], "vp") # Select 10th => 1 profile
  expect_s3_class(vpts[-1:-1933], "vp") # Remove 1933 => 1 profile left
  vpts_of_2 <- vpts[1:2]
  expect_s3_class(vpts_of_2[-1], "vp") # Remove 1st => 1 profile left
  expect_s3_class(vpts_of_2[-2], "vp") # Remove 2nd => 1 profile left
})
