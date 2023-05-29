test_that("regularize_vpts() returns error on incorrect parameters", {
  expect_error(regularize_vpts("not_a_vpts"))
  expect_error(regularize_vpts(example_vp))
  expect_error(regularize_vpts(example_vpts, "not_an_interval"))
  expect_error(regularize_vpts(example_vpts, -5))
  expect_error(regularize_vpts(example_vpts, units = "not_an_unit"))
  expect_error(regularize_vpts(example_vpts, units = 2))
  expect_error(regularize_vpts(example_vpts, fill = "not_a_logical"))
  expect_error(regularize_vpts(example_vpts, fill = c(TRUE, TRUE)))
  expect_error(regularize_vpts(example_vpts, fill = c(1, 1)))
  expect_error(regularize_vpts(example_vpts, verbose = "not_a_logical"))
  expect_error(regularize_vpts(example_vpts, verbose = 1))
  expect_error(regularize_vpts(example_vpts, verbose = c(TRUE, TRUE)))
  expect_error(regularize_vpts(example_vpts, keep_datetime = "not_a_logical"))
  expect_error(regularize_vpts(example_vpts, keep_datetime = 1))
  expect_error(regularize_vpts(example_vpts, keep_datetime = c(TRUE, TRUE)))
  expect_error(regularize_vpts(example_vpts, date_min = "not_a_POSIXct"))
  expect_error(regularize_vpts(example_vpts, date_max = "not_a_POSIXct"))
  expect_error(regularize_vpts(example_vpts,
    date_min = as.POSIXct("2016-06-01"),
    date_max = as.POSIXct("2016-05-01")
  ))
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
    date_max = as.POSIXct("2016-09-12", tz = "UTC")
  )$daterange, structure(c(1473465600, 1473638400),
    class = c("POSIXct", "POSIXt"), tzone = "UTC"
  ))
})
test_that("regularized vpts has the same data at the same or slightly shifted times ", {
  expect_equal(
    regularize_vpts(example_vpts, verbose = F)[regularize_vpts(example_vpts, verbose = F)$datetime %in% example_vpts$datetime[c(618, 950)]],
    example_vpts[c(618, 950)]
  )
  # check for match in records that are shifted by 60 seconds
  expect_equal(
    regularize_vpts(example_vpts, verbose = F)[regularize_vpts(example_vpts, verbose = F)$datetime %in% (example_vpts$datetime[c(801, 1515)] + 60)]$data,
    example_vpts[c(801, 1515)]$data
  )
})
