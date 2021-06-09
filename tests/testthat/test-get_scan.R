pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

test_that("get_scan() returns error on incorrect parameters", {
  expect_error(get_scan("not_a_pvol", 5), "`x` must be a `pvol` object.", fixed = TRUE)
  expect_error(get_scan(pvol, "not_numeric"), "`elev` must be numeric.", fixed = TRUE)
  expect_error(get_scan(pvol, 1:2))
  expect_error(get_scan(pvol, 1, c(T,F)))
  expect_error(get_scan(pvol, 1, 'a'))
})

test_that("get_scan() returns a object of class scan", {
  expect_s3_class(get_scan(pvol, 5), "scan")
})

test_that("get_scan() returns the scan closest to elev parameter", {
  # Elevation angles for example pvol are 0.5 1.5 2.5
  expect_equal(get_scan(pvol, 1.5)$geo$elangle, 1.5)
  expect_equal(get_scan(pvol, 1.9)$geo$elangle, 1.5)
  expect_warning(scan <- get_scan(pvol, 2), "multiple") # Lowest elangle is chosen if equal
  expect_equal(scan$geo$elangle, 1.5) # Lowest elangle is chosen if equal
  expect_silent(scan <- get_scan(pvol, 2, T)) # no warning if all scans are returned
  expect_type(scan,'list')
  expect_length(scan,2)
  expect_silent(scan <- get_scan(pvol, 1.5, T)) # no warning if all scans are returned
  expect_type(scan,'list')
  expect_length(scan,1)
  expect_equal(get_scan(pvol, 2.1)$geo$elangle, 2.5)
  expect_equal(get_scan(pvol, 40)$geo$elangle, 2.5)
})
