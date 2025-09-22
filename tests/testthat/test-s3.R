test_that("S3 bucket & prefix exist", {
  skip_if_offline()
  withr::local_options(timeout = 15)

  bucket <- "unidata-nexrad-level2"
  prefix <- "1998/01/20/KABR/"

  expect_true(s3_bucket_exists(bucket))
  expect_true(s3_prefix_exists(bucket, prefix, timeout_s = 10))
  expect_false(s3_prefix_exists(bucket, "bucket/that/does/not/exist/"))
})

test_that("s3_get_bucket_df parses an S3 listing", {
  skip_if_offline()
  withr::local_options(timeout = 20)

  df <- s3_get_bucket_df(
    bucket = "unidata-nexrad-level2",
    prefix = "1998/01/20/KABR/",
    max_keys = 10
  )

  # Property-based checks (avoid brittle exact matches)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Key","LastModified","Size","ETag","StorageClass") %in% names(df)))
  expect_true(nrow(df) >= 1)
  expect_true(inherits(df$LastModified, "POSIXct"))
  expect_true(all(is.finite(df$Size)))
  expect_true(all(startsWith(df$Key, "1998/01/20/KABR/")))
})

test_that("s3_save_object downloads an object and respects overwrite", {
  skip_if_offline()
  withr::local_options(timeout = 30)

  listing <- s3_get_bucket_df("unidata-nexrad-level2", "1998/01/20/KABR/", max_keys = 1)
  skip_if(nrow(listing) == 0, "No objects returned; historical dataset may have moved.")

  key <- listing$Key[[1]]
  tmp <- tempfile(); on.exit(unlink(tmp), add = TRUE)

  # first download (live)
  s3_save_object(object = key, bucket = "unidata-nexrad-level2", file = tmp, overwrite = TRUE)
  expect_true(file.exists(tmp))
  size1 <- file.size(tmp); expect_gt(size1, 0)

  # second call with overwrite=FALSE should leave file unchanged
  s3_save_object(object = key, bucket = "unidata-nexrad-level2", file = tmp, overwrite = FALSE)
  expect_identical(file.size(tmp), size1)
})

test_that("s3_get_bucket_df warns and clamps when max_keys > 1000", {
  expect_warning(
    {
      df <- bioRad:::s3_get_bucket_df(bucket = "s3://ignored",
                                      max_keys = 5000,
                                      max = 0)
      expect_s3_class(df, "data.frame")
      expect_identical(nrow(df), 0L)
    },
    "hard limit of 1000"
  )
})

test_that("s3_get_bucket_df warns and clamps when max_keys < 1", {
  expect_warning(
    {
      df <- bioRad:::s3_get_bucket_df(bucket = "s3://ignored",
                                      max_keys = 0,
                                      max = 0)
      expect_s3_class(df, "data.frame")
      expect_identical(nrow(df), 0L)
    },
    "must be >= 1"
  )
})
