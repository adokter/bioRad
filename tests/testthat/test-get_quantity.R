vp <- example_vp
vp_list_mixed <- list(example_vp, "not_a_vp")
vpts <- example_vpts

test_that("get_quantity() returns error on incorrect parameters", {
  expect_error(get_quantity("not_a_vp", "dens"))
  expect_error(
    get_quantity(vp_list_mixed, "dens"),
    "`x` must be list of `vp` objects.",
    fixed = TRUE
  )
  expect_error(
    get_quantity(vp, "not_a_quantity"),
    "Can't find quantity `not_a_quantity` in `x`",
    fixed = TRUE
  )
  expect_error(
    get_quantity(vpts, "not_a_quantity"),
    "Can't find quantity `not_a_quantity` in `x`",
    fixed = TRUE
  )

  # Quantities are case sensitive
  expect_error(get_quantity(vp, "dbzh"))
  expect_error(get_quantity(vp, "DENS"))
})

test_that("get_quantity.vp() returns correct quantity, processing eta, dbz, ff when below sd_vvp_threshold", {
  # Not tested for vp_list as that is a repetition of vp method

  # dens is returned as is
  dens <- vp$data$dens
  names(dens) <- vp$data$height # Add heights to make named vector
  expect_equal(get_quantity(vp, "dens"), dens)

  # height is returned as is
  height <- vp$data$height
  names(height) <- vp$data$height # Add heights to make named vector
  expect_equal(get_quantity(vp, "height"), height)

  # eta is set to 0 when below sd_vvp_threshold
  eta <- vp$data$eta
  eta[vp$data$sd_vvp < sd_vvp_threshold(vp)] <- 0
  names(eta) <- vp$data$height
  expect_equal(get_quantity(vp, "eta"), eta)

  # dbz is set to -Inf when below sd_vvp_threshold
  dbz <- vp$data$dbz
  dbz[vp$data$sd_vvp < sd_vvp_threshold(vp)] <- -Inf
  names(dbz) <- vp$data$height
  expect_equal(get_quantity(vp, "dbz"), dbz)

  # ff (not tested for u, v, w, dd is set to NaN when below sd_vvp_threshold
  ff <- vp$data$ff
  ff[vp$data$sd_vvp < sd_vvp_threshold(vp)] <- NaN
  names(ff) <- vp$data$height
  expect_equal(get_quantity(vp, "ff"), ff)
})

test_that("get_quantity.vpts() returns correct quantity, processing eta, dbz, ff when below sd_vvp_threshold", {

  # dens is returned as is
  dens <- vpts$data$dens
  rownames(dens) <- vpts$height
  colnames(dens) <- as.character(vpts$datetime)
  expect_equal(get_quantity(vpts, "dens"), dens)

  # height is returned as a matrix repetition of vpts$height
  height <- matrix(rep(as.numeric(vpts$height),dim(vpts)[1]), ncol=dim(vpts)[1])
  rownames(height) <- vpts$height
  colnames(height) <- as.character(vpts$datetime)
  expect_equal(get_quantity(vpts, "height"), height)

  # eta is set to 0 when below sd_vvp_threshold
  eta <- vpts$data$eta
  rownames(eta) <- vpts$height
  colnames(eta) <- as.character(vpts$datetime)
  eta[vpts$data$sd_vvp < sd_vvp_threshold(vpts)] <- 0
  expect_equal(get_quantity(vpts, "eta"), eta)

  # dbz is set to -Inf when below sd_vvp_threshold
  dbz <- vpts$data$dbz
  rownames(dbz) <- vpts$height
  colnames(dbz) <- as.character(vpts$datetime)
  dbz[vpts$data$sd_vvp < sd_vvp_threshold(vpts)] <- -Inf
  expect_equal(get_quantity(vpts, "dbz"), dbz)

  # ff (not tested for u, v, w, dd is set to NaN when below sd_vvp_threshold
  ff <- vpts$data$ff
  rownames(ff) <- vpts$height
  colnames(ff) <- as.character(vpts$datetime)
  ff[vpts$data$sd_vvp < sd_vvp_threshold(vpts)] <- NaN
  expect_equal(get_quantity(vpts, "ff"), ff)
})

test_that("get_quantity.vp() returns vectors for all 17 quantities", {
  expect_vector(get_quantity(vp, "height"))
  expect_vector(get_quantity(vp, "dens"))
  expect_vector(get_quantity(vp, "u"))
  expect_vector(get_quantity(vp, "v"))
  expect_vector(get_quantity(vp, "w"))
  expect_vector(get_quantity(vp, "ff"))
  expect_vector(get_quantity(vp, "dd"))
  expect_vector(get_quantity(vp, "sd_vvp"))
  expect_vector(get_quantity(vp, "gap"))
  expect_vector(get_quantity(vp, "dbz"))
  expect_vector(get_quantity(vp, "eta"))
  expect_vector(get_quantity(vp, "dens"))
  expect_vector(get_quantity(vp, "DBZH"))
  expect_vector(get_quantity(vp, "n"))
  expect_vector(get_quantity(vp, "n_all"))
  expect_vector(get_quantity(vp, "n_dbz"))
  expect_vector(get_quantity(vp, "n_dbz_all"))
})

test_that("get_quantity.vpts() return a matrix for all 17 quantities", {
  expect_true(is(get_quantity(vpts, "height"), "matrix"))
  expect_true(is(get_quantity(vpts, "dens"), "matrix"))
  expect_true(is(get_quantity(vpts, "u"), "matrix"))
  expect_true(is(get_quantity(vpts, "v"), "matrix"))
  expect_true(is(get_quantity(vpts, "w"), "matrix"))
  expect_true(is(get_quantity(vpts, "ff"), "matrix"))
  expect_true(is(get_quantity(vpts, "dd"), "matrix"))
  expect_true(is(get_quantity(vpts, "sd_vvp"), "matrix"))
  expect_true(is(get_quantity(vpts, "gap"), "matrix"))
  expect_true(is(get_quantity(vpts, "dbz"), "matrix"))
  expect_true(is(get_quantity(vpts, "eta"), "matrix"))
  expect_true(is(get_quantity(vpts, "dens"), "matrix"))
  expect_true(is(get_quantity(vpts, "DBZH"), "matrix"))
  expect_true(is(get_quantity(vpts, "n"), "matrix"))
  expect_true(is(get_quantity(vpts, "n_all"), "matrix"))
  expect_true(is(get_quantity(vpts, "n_dbz"), "matrix"))
  expect_true(is(get_quantity(vpts, "n_dbz_all"), "matrix"))
})
