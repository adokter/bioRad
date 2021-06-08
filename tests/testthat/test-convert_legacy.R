vp <- example_vp
vpts <- example_vpts

test_that("convert_legacy() returns error on incorrect parameters", {
  expect_error(convert_legacy("not_a_object"))
})

test_that("convert_legacy() returns same object type", {
  expect_s3_class(convert_legacy(vp), "vp")
  expect_s3_class(convert_legacy(vpts), "vpts")
})

test_that("convert_legacy() renames certain data colnames", {
  # Make legacy objects
  names(vp$data) <- sub("height", "HGHT", names(vp$data))
  names(vpts$data) <- sub("height", "heights", names(vpts$data))
  names(vpts$data) <- sub("dates", "datetime", names(vpts$data))

  expect_identical(convert_legacy(vp), example_vp)
  expect_identical(convert_legacy(vpts), example_vpts)
})
