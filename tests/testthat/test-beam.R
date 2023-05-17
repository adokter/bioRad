test_that("beam_distance() returns error on incorrect parameters", {
  expect_error(beam_distance(), 'argument "range" is missing, with no default')
  expect_error(beam_distance(1), 'argument "elev" is missing, with no default')
  expect_error(beam_distance(1:2, 1:3))
  expect_error(beam_distance(1, 2, k = "not_a_double"))
  expect_error(beam_distance(1, 2, k = NA))
  expect_error(beam_distance(1, 2, lat = "not_a_double"))
  expect_error(beam_distance(1, 2, lat = NA))
  expect_error(beam_distance(1, 2, re = "not_a_double"))
  expect_error(beam_distance(1, 2, re = NA))
  expect_error(beam_distance(1, 2, rp = "not_a_double"))
  expect_error(beam_distance(1, 2, rp = NA))
  expect_error(beam_distance(1, 2, k = 1:2))
  expect_error(beam_distance(1, 2, lat = 1:2))
  expect_error(beam_distance(1, 2, re = 1:2))
  expect_error(beam_distance(1, 2, rp = 1:2))
})

test_that("beam_height() returns error on incorrect parameters", {
  expect_error(beam_height(), 'argument "range" is missing, with no default')
  expect_error(beam_height(1), 'argument "elev" is missing, with no default')
  expect_error(beam_height(1:2, 1:3))
  expect_error(beam_height(1, 2, k = "not_a_double"))
  expect_error(beam_height(1, 2, k = NA))
  expect_error(beam_height(1, 2, lat = "not_a_double"))
  expect_error(beam_height(1, 2, lat = NA))
  expect_error(beam_height(1, 2, re = "not_a_double"))
  expect_error(beam_height(1, 2, re = NA))
  expect_error(beam_height(1, 2, rp = "not_a_double"))
  expect_error(beam_height(1, 2, rp = NA))
  expect_error(beam_height(1, 2, k = 1:2))
  expect_error(beam_height(1, 2, lat = 1:2))
  expect_error(beam_height(1, 2, re = 1:2))
  expect_error(beam_height(1, 2, rp = 1:2))
})

test_that("beam_range() returns error on incorrect parameters", {
  expect_error(beam_range(), 'argument "distance" is missing, with no default')
  expect_error(beam_range(1), 'argument "elev" is missing, with no default')
  expect_error(beam_range(1:2, 1:3))
  expect_error(beam_range(1, 2, k = "not_a_double"))
  expect_error(beam_range(1, 2, k = NA))
  expect_error(beam_range(1, 2, lat = "not_a_double"))
  expect_error(beam_range(1, 2, lat = NA))
  expect_error(beam_range(1, 2, re = "not_a_double"))
  expect_error(beam_range(1, 2, re = NA))
  expect_error(beam_range(1, 2, rp = "not_a_double"))
  expect_error(beam_range(1, 2, rp = NA))
  expect_error(beam_range(1, 2, k = 1:2))
  expect_error(beam_range(1, 2, lat = 1:2))
  expect_error(beam_range(1, 2, re = 1:2))
  expect_error(beam_range(1, 2, rp = 1:2))
})

test_that("beam_profile() returns error on incorrect parameters", {
  expect_error(beam_profile(), 'argument "height" is missing, with no default')
  expect_error(beam_profile(1), 'argument "distance" is missing, with no default')
  expect_error(beam_profile(1, 1), 'argument "elev" is missing, with no default')
  expect_error(beam_profile(1:2, 1:3,1))
  expect_error(beam_profile(1, 2, k = "not_a_double"))
  expect_error(beam_profile(1, 2, k = NA))
  expect_error(beam_profile(1, 2, lat = "not_a_double"))
  expect_error(beam_profile(1, 2, lat = NA))
  expect_error(beam_profile(1, 2, re = "not_a_double"))
  expect_error(beam_profile(1, 2, re = NA))
  expect_error(beam_profile(1, 2, rp = "not_a_double"))
  expect_error(beam_profile(1, 2, rp = NA))
  expect_error(beam_profile(1, 2, k = 1:2))
  expect_error(beam_profile(1, 2, lat = 1:2))
  expect_error(beam_profile(1, 2, re = 1:2))
  expect_error(beam_profile(1, 2, rp = 1:2))
})

test_that("beam_distance() returns expected values", {
  expect_equal(beam_distance(100000, 5), 99495.125492)
  expect_equal(beam_distance(100000 * 2:3, 5), c(198721.311636, 297648.555804))
  expect_equal(beam_distance(100000, 2:3), c(99885.897399, 99785.935245))
  expect_equal(beam_distance(100000 * c(NA, 2:3), 5), c(NA, 198721.311636, 297648.555804))
  expect_equal(beam_distance(100000, c(2:3, NA)), c(99885.897399, 99785.935245, NA))
  expect_equal(beam_distance(100000 * 1:2, c(2, 5)), c(99885.897399, 198721.311636))
})

test_that("beam_height() returns expected values", {
  expect_equal(beam_height(100000, c(2:3, NA)), c(4077.563707055, 5820.194448376, NA))
  expect_equal(beam_height(100000 * 1:2, c(2, 5)), c(4077.563707055, 19762.53126097))
})

test_that("beam_range() returns expected values", {
  expect_equal(beam_range(100000, c(2:3, NA)), c(100114.2999242, 100214.7013404, NA))
  expect_equal(beam_range(100000 * 1:2, c(2, 5)), c(100114.2999242, 201290.5648873))
})

test_that("beam_distance() returns expected values for different input", {
  expect_lt(beam_distance(100000, 6), beam_distance(100000, 5))
  expect_lt(beam_distance(100000, 5), beam_distance(100000, 5, rp = 7000))
  expect_lt(beam_distance(100000, 5), beam_distance(100000, 5, re = 7000))
  expect_lt(beam_distance(100000, 5), beam_distance(100000, 5, k = 1.5))
  expect_equal(
    beam_distance(100000, 5, re = 7000, rp = 7000),
    beam_distance(100000, 5, re = 7000, rp = 7000, lat = 50)
  )
})

test_that("beam_range() returns expected values for different input", {
  expect_gt(beam_range(100000, 6), beam_range(100000, 5))
  expect_gt(beam_range(100000, 5), beam_range(100000, 5, rp = 7000))
  expect_gt(beam_range(100000, 5), beam_range(100000, 5, re = 7000))
  expect_gt(beam_range(100000, 5), beam_range(100000, 5, k = 1.5))
  expect_equal(
    beam_range(100000, 5, re = 7000, rp = 7000),
    beam_range(100000, 5, re = 7000, rp = 7000, lat = 50)
  )
})

test_that("Distance can be calculated from range and vice versa", {
  expect_equal(beam_range(beam_distance(90000 * 1:2, elev = 3), elev = 3), 90000 * 1:2)
  expect_equal(beam_range(beam_distance(90000 * 1:2, elev = 3:4), elev = 3:4), 90000 * 1:2)
})

test_that("Distance equals range when beam is following the earth (elev = 0)", {
  expect_equal(beam_height(10000, 0, k = 10e7), 0)
  expect_equal(beam_distance(10000, 0, k = 10e7), 10000, tolerance = .000001)
  expect_equal(beam_range(10000, 0, k = 10e7), 10000, tolerance = .000001)
})
