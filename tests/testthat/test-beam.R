test_that("beam_distance() returns error on incorrect parameters", {
  expect_error(
    beam_distance(),
    regexp = 'argument "range" is missing, with no default',
    fixed = TRUE)
  expect_error(
    beam_distance(1),
    regexp = 'argument "elev" is missing, with no default',
    fixed = TRUE)
  expect_error(
    beam_distance(1:2, 1:3),
    regex = "`range` and `elev` should either be equal length or either should have a length of one",
    fixed = TRUE)
  expect_error(
    beam_distance(1, 2, k = "not_a_double"),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE)
  expect_error(
    beam_distance(1, 2, k = NA),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE)
  expect_error(
    beam_distance(1, 2, lat = "not_a_double"))
  expect_error(
    beam_distance(1, 2, lat = NA))
  expect_error(
    beam_distance(1, 2, re = "not_a_double"))
  expect_error(
    beam_distance(1, 2, re = NA),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE)
  expect_error(
    beam_distance(1, 2, rp = "not_a_double"))
  expect_error(
    beam_distance(1, 2, rp = NA),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE)
  expect_error(
    beam_distance(1, 2, k = 1:2),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE)
  expect_error(
    beam_distance(1, 2, lat = 1:2),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE)
  expect_error(
    beam_distance(1, 2, re = 1:2),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE)
  expect_error(
    beam_distance(1, 2, rp = 1:2),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE)
})

test_that("beam_height() returns error on incorrect parameters", {
  expect_error(
    beam_height(),
    regexp = 'argument "range" is missing, with no default',
    fixed = TRUE)
  expect_error(
    beam_height(1),
    regexp = 'argument "elev" is missing, with no default',
    fixed = TRUE)
  expect_error(
    beam_height(1:2, 1:3),
    regexp = "`range` and `elev` should either be equal length or either should have a length of one",
    fixed = TRUE)
  expect_error(
    beam_height(1, 2, k = "not_a_double"),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, k = NA),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, lat = "not_a_double"),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, lat = NA),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, re = "not_a_double"),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, re = NA),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, rp = "not_a_double"),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, rp = NA),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, k = 1:2),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, lat = 1:2),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, re = 1:2),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_height(1, 2, rp = 1:2),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
    )
})

test_that("beam_range() returns error on incorrect parameters", {
  expect_error(
    beam_range(),
    regexp = 'argument "distance" is missing, with no default',
    fixed = TRUE
    )
  expect_error(
    beam_range(1),
    regexp = 'argument "elev" is missing, with no default',
    fixed = TRUE
    )
  expect_error(
    beam_range(1:2, 1:3),
    regexp = "`distance` and `elev` should either be equal length or either should have a length of one",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, k = "not_a_double"),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, k = NA),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, lat = "not_a_double"),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, lat = NA),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, re = "not_a_double"),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, re = NA),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, rp = "not_a_double"),
    regexp = "",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, rp = NA),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, k = 1:2),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, lat = 1:2),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, re = 1:2),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_range(1, 2, rp = 1:2),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
    )
})

test_that("beam_profile() returns error on incorrect parameters", {
  expect_error(
    beam_profile(),
    regexp = 'argument "height" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    beam_profile(1),
    regexp = 'argument "distance" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    beam_profile(1, 1),
    regexp = 'argument "elev" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    beam_profile(1:2, 1:3,1),
    regexp = "`height` and `distance` not have an unequal length when more then one.",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, k = "not_a_double", elev = 4),
    regexp = 'k is not a number (a length one numeric vector).',
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, k = NA, elev = 4),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, lat = "not_a_double", elev = 4),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, lat = NA, elev = 4),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, re = "not_a_double", elev = 5),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, re = NA, elev = 4),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, rp = "not_a_double", elev = 4),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, rp = NA, elev = 4),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, k = 1:2, elev = 4),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, lat = 1:2, elev = 4),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, re = 1:2, elev = 42),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
    )
  expect_error(
    beam_profile(1, 2, rp = 1:2, elev = 10E9),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
    )
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

test_that("beam_profile_overlap() returns error on incorrect parameters", {
  expect_error(
    beam_profile_overlap(example_vp),
    regexp = 'argument "elev" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(example_vp, elev = "a"),
    regexp = "'elev' must be a number or numeric vector with the beam elevation(s)",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap('a', elev = c(1:3)),
    regexp = "'vp' must be an object of class vp",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(example_vp, elev = 1:4, distance = c(-2:4)),
    regexp = "'distance' should be a positive numeric value or vector",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(example_vp, elev = 1:4, distance = "a"),
    regexp = "'distance' should be a positive numeric value or vector",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 90:100,
      zlim = c(1, 9, 14)
    ),
    regexp = "'zlim' should be a numeric vector of length two",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 90:100,
      zlim = c(1, "a", 14)
    ),
    regexp = "'zlim' should be a numeric vector of length two",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 90:100,
      zlim = 12
    ),
    regexp = "'zlim' should be a numeric vector of length two",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 90:100,
      zlim = c("a","b")
    ),
    regexp = "'zlim' should be a numeric vector of length two",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 90:100,
      zlim = "a"
    ),
    regexp = "'zlim' should be a numeric vector of length two",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 90:100,
      zlim = c(NA, 1)
    ),
    regexp = "'zlim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 90:100,
      zlim = c(2, NA)
    ),
    regexp = "'zlim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 90:100,
      zlim = c(42, 1)
    ),
    regexp = "'zlim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 80:90,
      steps = c(4,2)
    ),
    regexp = "'step' should be a numeric value",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 80:90,
      steps = "a"
    ),
    regexp = "'step' should be a numeric value",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp,
      elev = 1:4,
      distance = 70:80,
      quantity = "not_a_valid_quanity"
    ),
    regexp = "'quantity' should be one of 'dens' or 'eta'",
    fixed = TRUE
  )
  example_vp_no_antenna <- example_vp
  example_vp_no_antenna$attributes$where$height <- NULL
  expect_error(
    beam_profile_overlap(
      example_vp_no_antenna,
      elev = 1:4,
      distance = 1:3
    ),
    regexp = "antenna height cannot be found in polar volume, specify antenna height using 'antenna' argument",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp_no_antenna,
      elev = 1:4,
      distance = 1:3,
      antenna = c(4,2)
    ),
    regexp = "antenna is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp_no_antenna,
      elev = 1:4,
      distance = 1:3,
      antenna = "not_a_number"
    ),
    regexp = "antenna is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  example_vp_no_lat <- example_vp
  example_vp_no_lat$attributes$where$lat <- NULL
  expect_error(
    beam_profile_overlap(
      example_vp_no_lat,
      elev = 1:4,
      distance = 1:3
    ),
    regexp = "radar latitude cannot be found in polar volume, specify using 'lat' argument",
    fixed = TRUE
  )
  expect_error(
    beam_profile_overlap(
      example_vp_no_lat,
      elev = 1:4,
      distance = 1:3,
      lat = "b"
    ),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
  )
})

test_that("beam_width() returns error on incorrect parameters", {
  range <- seq(0, 100000, 100)
  expect_error(
    beam_width(c(range,"a")),
    regexp = "range is not a numeric or integer vector",
    fixed = TRUE
  )
  expect_error(
    beam_width(range,
               beam_angle = "a")
  )
})


test_that("gaussian_beam_profile returns error on incorrect parameters", {
  range <- seq(0, 100000, 100)
  expect_error(
    gaussian_beam_profile("a"),
    regexp = "height is not a numeric or integer vector",
    fixed = TRUE
    )
  expect_error(
    gaussian_beam_profile(3),
    regexp = 'argument "range" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range = -2:3, elev = 2),
    regexp = 'range must be positive',
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3,range),
    regexp = 'argument "elev" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1:4),
    regexp = "elev is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = "a"),
    regexp = "elev is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, antenna = 1:4),
    regexp = "antenna is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, antenna = list(1)),
    regexp = "antenna is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, antenna = "a"),
    regexp = "antenna is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, beam_angle = c(0.2,4)),
    regexp = "beam_angle is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, beam_angle = -9),
    regexp = "beam_angle must be positive.",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, beam_angle = NA),
    regexp = "beam_angle is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, beam_angle = Inf),
    regexp = "beam_angle can't be infinite.",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, k = -4:2),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, k = Inf),
    regexp = "k can't be infinite.",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, k = "a"),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, lat = 180),
    regexp = "lat not less than or equal to 90",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, lat = -180),
    regexp = "lat not greater than or equal to -90",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, lat = seq(5)),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, lat = NA),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, rp = seq(2)),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, rp = "a"),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, rp = Inf),
    regexp = "rp can't be infinite",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, re = seq(2)),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, re = "a"),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    gaussian_beam_profile(3, range, elev = 1.44, re = Inf),
    regexp = "re can't be infinite.",
    fixed = TRUE
  )
})
