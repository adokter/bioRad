test_that("color_scale() returns error on incorrect parameters", {
  expect_error(
    color_scale(param = c("a")),
    regexp = 'argument "zlim" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    color_scale(param = c("a","b"), zlim = -4),
    regexp = 'the condition has length > 1',
    fixed = TRUE
  )
})

test_that("add_color_transparency() returns error on incorrect parameters", {
  expect_error(
    add_color_transparency(),
    regexp = "Please provide a vector or matrix of colours.",
    fixed = TRUE
  )
  expect_error(
    add_color_transparency(c("red","green","not_a_colour")),
    regexp = "invalid color name 'not_a_colour'",
    fixed = TRUE
  )
})
