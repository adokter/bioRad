test_that("regularize_vpts() returns error on incorrect parameters", {
  expect_error(regularize_vpts("not_a_vpts"),
               regexp = "is.vpts(x = ts) is not TRUE",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vp),
               regexp = "is.vpts(x = ts) is not TRUE",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, "not_an_interval"),
               regexp = "interval is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, -5),
               regexp = "interval not greater than 0",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, units = "not_an_unit"),
               regexp = "Invalid 'units' argument. Should be one ofc('secs', 'mins', 'hours','days', 'weeks')",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, units = c("weeks", "days")),
               regexp = "Only one 'units' argument can be provided.",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, units = 2),
               regexp = "Invalid 'units' argument. Should be one ofc('secs', 'mins', 'hours','days', 'weeks')",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, fill = "not_a_logical", verbose = FALSE),
               regexp = "fill is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, fill = c(TRUE, TRUE), verbose = FALSE))
  expect_error(regularize_vpts(example_vpts, fill = c(1, 1), verbose = FALSE),
               regexp = "fill is not a number (a length one numeric vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, verbose = "not_a_logical"),
               regexp = "verbose is not a flag (a length one logical vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, verbose = 1),
               regexp = "verbose is not a flag (a length one logical vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, verbose = c(TRUE, TRUE)),
               regexp = "verbose is not a flag (a length one logical vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, keep_datetime = "not_a_logical"),
               regexp = "keep_datetime is not a flag (a length one logical vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, keep_datetime = 1),
               regexp = "keep_datetime is not a flag (a length one logical vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, keep_datetime = c(TRUE, TRUE)),
               regexp = "keep_datetime is not a flag (a length one logical vector).",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, date_min = "not_a_POSIXct", verbose = FALSE),
               regexp = "date_min is not a POSIXt date-time object",
               fixed = TRUE)
  expect_error(regularize_vpts(example_vpts, date_max = "not_a_POSIXct", verbose = FALSE),
               regexp = "date_max is not a POSIXt date-time object",
               fixed = TRUE)
  expect_error(
    regularize_vpts(
      example_vpts,
      date_min = as.POSIXct("2016-06-01"),
      date_max = as.POSIXct("2016-05-01"),
      verbose = FALSE
      ),
    regexp = "date_max not greater than or equal to date_min",
    fixed = TRUE
    )
})
test_that("`regularize_vpts` has expected datetime when dates and intervals are manipulated", {
  expect_message(expect_equal(
    length(regularize_vpts(example_vpts)$datetime),
    2737
  ))
  expect_silent(expect_equal(unique(as.numeric(
    diff(
      regularize_vpts(example_vpts, verbose = FALSE)$datetime
    ),
    unit = "secs"
  )), 300))
  expect_equal(
    unique(as.numeric(diff(regularize_vpts(example_vpts,
      verbose = FALSE, interval = 600
    )$datetime), unit = "secs")),
    600
  )
  expect_equal(
    unique(as.numeric(diff(
      regularize_vpts(example_vpts,
        verbose = FALSE,
        interval = 15,
        units = "mins"
      )$datetime
    ), unit = "secs")),
    900
  )
  expect_equal(regularize_vpts(example_vpts,
    date_min = as.POSIXct("2016-09-10", tz = "UTC"),
    date_max = as.POSIXct("2016-09-12", tz = "UTC"),
    verbose = FALSE
  )$daterange, structure(c(1473465600, 1473638400),
    class = c("POSIXct", "POSIXt"), tzone = "UTC"
  ))
})
test_that("regularized vpts has the same data at the same or slightly shifted times ", {
  expect_equal(
    regularize_vpts(
      example_vpts,
      verbose = FALSE)[regularize_vpts(
        example_vpts,
        verbose = FALSE)$datetime %in%
          example_vpts$datetime[c(618, 950)]],
    example_vpts[c(618, 950)]
  )
  # check for match in records that are shifted by 60 seconds
  expect_equal(
    regularize_vpts(
      example_vpts,
      verbose = FALSE)[regularize_vpts(
        example_vpts,
        verbose = FALSE)$datetime %in%
          (example_vpts$datetime[c(801, 1515)] + 60)]$data,
    example_vpts[c(801, 1515)]$data)
})
