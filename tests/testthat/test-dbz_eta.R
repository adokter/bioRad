test_that("dbz_to_eta() returns error on incorrect parameters", {
  test_that("dbz_to_eta() returns error on incorrect parameters", {
    expect_error(dbz_to_eta('not_a_dbz', 10),"dbz is not a numeric or integer vector", fixed = TRUE)
    expect_error(dbz_to_eta(7, 'not_a_wavelength'), "`wavelength` must be a strictly positive numeric.", fixed = TRUE)
    expect_error(dbz_to_eta(7, 10,'not_a_K'), "`K` must be a strictly positive numeric.", fixed = TRUE)
  })

  test_that("dbz_to_eta() returns the correct eta", {
    expect_equal(dbz_to_eta(7, 5), 2282.193, tolerance = 0.0001)
  })
})

test_that("eta_to_dbz() returns error on incorrect parameters", {
  test_that("eta_to_dbz() returns error on incorrect parameters", {
    expect_error(eta_to_dbz('not_a_eta', 10),"`eta` must be a strictly positive numeric.", fixed = TRUE)
    expect_error(eta_to_dbz(10000, 'not_a_wavelength'), "`wavelength` must be a strictly positive numeric.", fixed = TRUE)
    expect_error(eta_to_dbz(10000, 10,'not_a_K'), "`K` must be a strictly positive numeric.", fixed = TRUE)
    expect_error(eta_to_dbz(-10, 10),"`eta` must be a strictly positive numeric.", fixed = TRUE)
  })

  test_that("eta_to_dbz() returns the correct dbz", {
    expect_equal(eta_to_dbz(10000, 10), 25.45768, tolerance = 0.0001)
  })
})

