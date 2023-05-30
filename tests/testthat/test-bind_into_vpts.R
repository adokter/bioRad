test_that("bind_into_vpts() returns error on incorrect parameters", {
  # non vpts inputs
  expect_error(
    bind_into_vpts(
      example_vpts[2:5],
      example_vpts[1],
      "not_a_vpts",
      example_vpts[6:9]
    ),
    regexp = "requires vpts objects as input",
    fixed = TRUE
  )
  # non vp objects input
  expect_error(
    bind_into_vpts.vp(
      example_vpts[2:5],
      example_vpts[1]
    ),
    regexp = "requires vp objects as input",
    fixed = TRUE
  )
  # unexpected objects in list
  expect_error(
    suppressWarnings(
      bind_into_vpts(list("a", example_vpts[2:5]))
      ),
    regexp = "requires list of vp objects as input",
    fixed = TRUE
  )
  # must have same radar
  example_vpts_radar_modified <- example_vpts
  example_vpts_radar_modified$radar <- "seang"
  expect_error(
    bind_into_vpts(example_vpts, example_vpts_radar_modified),
    regexp = "Vertical profiles are not from a single radar",
    fixed = TRUE
  )
  # different quantities in vertical profiles
  example_vpts_dbzh_delete <- example_vpts
  example_vpts_dbzh_delete$data$DBZH <- NULL
  expect_error(
    bind_into_vpts(example_vpts_dbzh_delete, example_vpts),
    regexp = "Vertical profiles have different quantities",
    fixed = TRUE
  )
  # Vertical profiles with different altitude layer widths
  example_vpts_interval_edit <- example_vpts
  example_vpts_interval_edit$attributes$where$interval <- 189
  expect_error(
    bind_into_vpts(example_vpts_interval_edit, example_vpts),
    regexp = "Vertical profiles with different altitude layer widths",
    fixed = TRUE
  )
  # Vertical profiles with different numbers of altitude layers
  example_vpts_layer_edit <- example_vpts
  example_vpts_layer_edit$attributes$where$levels <- 23
  expect_error(
    bind_into_vpts(example_vpts_layer_edit, example_vpts),
    regexp = "Vertical profiles with different numbers of altitude layers",
    fixed = TRUE
  )
  # vp: Vertical profiles of radar contain different quantities.
  example_vp_dbzh_delete <- example_vp
  example_vp_dbzh_delete$data$DBZH <- NULL
  expect_error(
    bind_into_vpts(list(example_vp_dbzh_delete, example_vp)),
    regexp = "Vertical profiles of radar seang contain different quantities.",
    fixed = TRUE
  )
  # vp: Vertical profiles with different altitude layer widths
  example_vp_interval_edit <- example_vp
  example_vp_interval_edit$attributes$where$interval <- 189
  expect_error(
    bind_into_vpts(list(example_vp_interval_edit, example_vp)),
    regexp = "Vertical profiles with different altitude layer widths",
    fixed = TRUE
  )
  # vp: Vertical profiles with different numbers of altitude layers
  example_vp_layer_edit <- example_vp
  example_vp_layer_edit$attributes$where$levels <- 23
  expect_error(
    bind_into_vpts(list(example_vp_layer_edit, example_vp)),
    regexp = "Vertical profiles with different numbers of altitude layers",
    fixed = TRUE
  )
})

test_that("bind_into_vpts() warns for multiple different radars", {
  expect_warning(
    bind_into_vpts(
      example_vp,
      example_vpts[1]
    ),
    regexp = "Vertical profiles are not from a single radar.",
    fixed = TRUE
  )
})

test_that("vpts based on vp, multiple vp or vector of vp", {
  data(example_vpts)
  expect_s3_class(bind_into_vpts(example_vpts[1]), "vpts")
  expect_s3_class(bind_into_vpts(example_vpts[1], example_vpts[2]), "vpts")
  expect_s3_class(bind_into_vpts(c(example_vpts[1], example_vpts[2], example_vpts[3])), "vpts")
})

test_that("vpts based on vpts or multiple vpts", {
  data(example_vpts)
  expect_s3_class(bind_into_vpts(example_vpts[1:10]), "vpts")
  expect_s3_class(bind_into_vpts(example_vpts[1:10], example_vpts[11:20]), "vpts")
})

test_that("vp with different heights", {
  data(example_vp)
  lower <- example_vp
  lower$data <- lower$data[1:20, ]
  lower$attributes$where$levels = 20
  higher <- example_vp
  higher$data <- higher$data[3:25, ]
  lower$attributes$where$levels = 23
  expect_s3_class(bind_into_vpts(example_vp, lower), "vpts")
  expect_s3_class(bind_into_vpts(list(example_vp, higher)), "vpts")
  expect_s3_class(bind_into_vpts(example_vp, higher), "vpts")
  vpts <- bind_into_vpts(lower, higher)
  expect_s3_class(vpts, "vpts")
  expect_equal(vpts$attributes$where$levels, example_vpts$attributes$where$levels)
  higher$data$height <- higher$data$height + 1
  expect_error(bind_into_vpts(example_vp, higher))
})

test_that("vpts with different heights", {
  data(example_vpts)
  lower <- example_vpts
  lower$height <- lower$height[1:20]
  lower$attributes$where$levels = 20
  lower$data <- lapply(lower$data, head, 20)
  vpts <- bind_into_vpts(lower, example_vpts)
  expect_s3_class(vpts,'vpts')
  expect_equal(vpts$attributes$where$levels, example_vpts$attributes$where$levels)
})

test_that("vplist_to_vpts() returns error for unknown radar", {
  expect_error(
    vplist_to_vpts(list(example_vp,example_vp), radar = "not_a_radar"),
    regexp = "no profiles found for radar not_a_radar",
    fixed = TRUE
  )
})

test_that("vplist_to_vpts() returns vpts object", {
  expect_type(
    vplist_to_vpts(list(example_vp, example_vp), radar = "seang"),
    "list")
  expect_s3_class(
    vplist_to_vpts(list(example_vp, example_vp), radar = "seang"),
    "vpts"
  )
})

test_that("combined_heights() returns error on incorrect parameters", {
  expect_error(
    combined_heights("a"),
    regexp = "x is not a list",
    fixed = TRUE
  )
  expect_error(
    combined_heights(as.list(example_vpts$height)),
    regexp = "Not all data has the same size of altitude bins",
    fixed = TRUE
  )
})


