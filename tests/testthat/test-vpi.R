vpts <- example_vpts
vpi <- integrate_profile(vpts)

# No tests for error on incorrect parameters:
# summary() is generic and works for every input
# is.vpi() returns TRUE/FALSE and works for every input

test_that("summary.vpi() prints metadata to the console", {
  # print.vpi() will just print the data.frame to the console
  expect_output(summary(vpi), "Vertically integrated profile(s) (class vpi)", fixed = TRUE)
  expect_output(summary(vpi), "radar:", fixed = TRUE)
  expect_output(summary(vpi), "# profiles:", fixed = TRUE)
  expect_output(summary(vpi), "time range (UTC):", fixed = TRUE)
})

test_that("is.vpi() returns TRUE/FALSE correctly", {
  expect_true(is.vpi(vpi))
  expect_false(is.vpi("not_a_vpts"))
  expect_false(is.vpi(vpts))
})
