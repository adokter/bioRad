# Make legacy objects
legacy_vp <- example_vp
legacy_vpts <- example_vpts
names(legacy_vp$data) <- sub("height", "HGHT", names(legacy_vp$data))
names(legacy_vpts$data) <- sub("height", "heights", names(legacy_vpts$data))
names(legacy_vpts$data) <- sub("dates", "datetime", names(legacy_vpts$data))

test_that("convert_legacy() returns error on incorrect parameters", {
  expect_error(convert_legacy("not_a_object"))
})

test_that("convert_legacy() returns same object type", {
  expect_s3_class(convert_legacy(legacy_vp), "vp")
  expect_s3_class(convert_legacy(legacy_vpts), "vpts")
})

test_that("convert_legacy() renames certain data colnames", {
  expect_identical(convert_legacy(legacy_vp), example_vp)
  expect_identical(convert_legacy(legacy_vpts), example_vpts)
})
