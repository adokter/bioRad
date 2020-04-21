pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

test_that("returns error on incorrect parameters", {
  expect_error(get_scan("not_a_pvol", 5), "`x` must be a pvol object.")
  expect_error(get_scan(pvol, "not_a_double"), "`elev` must be numeric.")
})

test_that("get_scan() returns a object of class scan", {
  expect_s3_class(get_scan(pvol, 5), "scan")
})

test_that("get_scan() returns the scan closest to elev parameter", {
  scan_2 <- get_scan(pvol, 2)
  all_angels <- get_elevation_angles(pvol)

  expect_equal(scan2$geo$elangle,
               angels[which.min(abs(get_elevation_angles(pvol)-2))])
})
