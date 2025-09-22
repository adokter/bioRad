skip_if_offline()
temp_dir <- tempdir()
download.file(
  "https://unidata-nexrad-level2.s3.amazonaws.com/2019/10/01/KBGM/KBGM20191001_000542_V06",
  file.path(temp_dir, "KBGM_example"),
  quiet = TRUE
)


test_that("apply_mistnet() forwards errors from rhdf5", {
  skip_if_no_mistnet()
  expect_error(
    apply_mistnet(tempfile()),
    regexp = "does not exist"
  )
})

test_that("apply_mistnet() returns error on incorrect parameters", {
  skip_if_no_mistnet()
  expect_error(
    suppressMessages(
      apply_mistnet(
        file.path(temp_dir, "KBGM_example"),
        mistnet_elevations = "a"
      )
    ),
    regexp = "mistnet_elevations is not a numeric or integer vector"
  )
  expect_error(
    suppressMessages(
      apply_mistnet(
        file.path(temp_dir, "KBGM_example"),
        mistnet_elevations = 1:4
      )
    ),
    regexp = "length(mistnet_elevations) not equal to 5",
    fixed = TRUE
  )
  expect_error(
    suppressMessages(
      apply_mistnet(
        file.path(temp_dir, "KBGM_example"),
        mistnet_elevations = c(1:4, "b")
      )
    ),
    regexp = "mistnet_elevations is not a numeric or integer vector"
  )
  expect_error(
    suppressMessages(
      apply_mistnet(
        file.path(temp_dir, "KBGM_example"),
        pvolfile_out = tempfile(tmpdir = "not_a_real_dir")
      )
    ),
    regexp = "output directory not_a_real_dir not found"
  )
})
