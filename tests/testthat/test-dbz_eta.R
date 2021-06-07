test_that("dbz_to_eta() returns error on incorrect parameters", {
  test_that("dbz_to_eta() returns error on incorrect parameters", {
    expect_error(dbz_to_eta('not_a_dbz', 10))
    expect_error(dbz_to_eta(7, 'not_a_wavelength'))
    expect_error(dbz_to_eta(7, 10,'not_a_K'))
  })

  test_that("dbz_to_eta() returns the correct eta", {
    expect_equal(dbz_to_eta(7, 5), 2122.439, tolerance = 0.0001)
  })
})

test_that("eta_to_dbz() returns error on incorrect parameters", {
  test_that("eta_to_dbz() returns error on incorrect parameters", {
    expect_error(eta_to_dbz('not_a_eta', 10))
    expect_error(eta_to_dbz(10000, 'not_a_wavelength'))
    expect_error(eta_to_dbz(10000, 10,'not_a_K'))
  })

  test_that("eta_to_dbz() returns the correct dbz", {
    expect_equal(eta_to_dbz(10000, 10), 25.77285, tolerance = 0.0001)
  })
})

