vp <- example_vp
vp_list_mixed <- list(example_vp, "not_a_vp")
vpts <- example_vpts

test_that("get_quantity() returns error on incorrect parameters", {
  expect_error(get_quantity("not_a_vp", "dens"))
  expect_error(get_quantity(vp_list_mixed, "dens"), "`x` must be list of vp objects.", fixed = TRUE)
  expect_error(get_quantity(vp, "not_a_quantity"), "Can't find quantity `not_a_quantity` in `x`", fixed = TRUE)
  expect_error(get_quantity(vpts, "not_a_quantity"), "Can't find quantity `not_a_quantity` in `x`", fixed = TRUE)

  # Height is not a quantity
  expect_error(get_quantity(vp, "height"))
  expect_error(get_quantity(vpts, "height"))

  # Quantities are case sensitive
  expect_error(get_quantity(vp, "dbzh"))
  expect_error(get_quantity(vp, "DENS"))
})

test_that("get_quantity returns correct quantities from vp", {
  dens <- vp$data$dens
  names(dens) <- vp$data$height # Add heights to make named vector
  expect_equal(get_quantity(vp, "dens"), dens)

  eta <- vp$data$eta
  eta[vp$data$sd_vvp < sd_vvp_threshold(vp)] <- 0 # Set eta to 0 when below sd_vvp_threshold
  names(eta) <- vp$data$height # Add heights to make named vector
  expect_equal(get_quantity(vp, "eta"), eta)

  dbz <- vp$data$dbz
  dbz[vp$data$sd_vvp < sd_vvp_threshold(vp)] <- -Inf # Set dbz to -Inf when below sd_vvp_threshold
  names(dbz) <- vp$data$height # Add heights to make named vector
  expect_equal(get_quantity(vp, "dbz"), dbz)

  ff <- vp$data$ff
  ff[vp$data$sd_vvp < sd_vvp_threshold(vp)] <- NaN # Set ff to NaN when below sd_vvp_threshold
  names(ff) <- vp$data$height # Add heights to make named vector
  expect_equal(get_quantity(vp, "ff"), ff)
})

test_that("get_quantity returns correct quantities from vpts", {
  dens <- vpts$data$dens
  rownames(dens) <- vpts$height
  colnames(dens) <- as.character(vpts$datetime)
  expect_equal(get_quantity(vpts, "dens"), dens)

  eta <- vpts$data$eta
  rownames(eta) <- vpts$height
  colnames(eta) <- as.character(vpts$datetime)
  eta[vpts$data$sd_vvp < sd_vvp_threshold(vpts)] <- 0 # Set eta to 0 when below sd_vvp_threshold
  expect_equal(get_quantity(vpts, "eta"), eta)

  dbz <- vpts$data$dbz
  rownames(dbz) <- vpts$height
  colnames(dbz) <- as.character(vpts$datetime)
  dbz[vpts$data$sd_vvp < sd_vvp_threshold(vpts)] <- -Inf # Set dbz to -Inf when below sd_vvp_threshold
  expect_equal(get_quantity(vpts, "dbz"), dbz)

  ff <- vpts$data$ff
  rownames(ff) <- vpts$height
  colnames(ff) <- as.character(vpts$datetime)
  ff[vpts$data$sd_vvp < sd_vvp_threshold(vpts)] <- NaN # Set ff to NaN when below sd_vvp_threshold
  expect_equal(get_quantity(vpts, "ff"), ff)
})

test_that("get_quantity returns vectors for all quantities, vp", {

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

test_that("get_quantity returns vectors for all quantities, vpts", {

  expect_vector(get_quantity(vpts, "dens"))
  expect_vector(get_quantity(vpts, "u"))
  expect_vector(get_quantity(vpts, "v"))
  expect_vector(get_quantity(vpts, "w"))
  expect_vector(get_quantity(vpts, "ff"))
  expect_vector(get_quantity(vpts, "dd"))
  expect_vector(get_quantity(vpts, "sd_vvp"))
  expect_vector(get_quantity(vpts, "gap"))
  expect_vector(get_quantity(vpts, "dbz"))
  expect_vector(get_quantity(vpts, "eta"))
  expect_vector(get_quantity(vpts, "dens"))
  expect_vector(get_quantity(vpts, "DBZH"))
  expect_vector(get_quantity(vpts, "n"))
  expect_vector(get_quantity(vpts, "n_all"))
  expect_vector(get_quantity(vpts, "n_dbz"))
  expect_vector(get_quantity(vpts, "n_dbz_all"))
})
