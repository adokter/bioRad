vp <- example_vp
vpts <- example_vpts
vpi <- integrate_profile(example_vpts)
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

test_that("doy_noy() returns error on incorrect parameters", {
  expect_error(doy("not_a_object"))
  expect_error(noy("not_a_object"))

})

test_that("doy_noy() returns a number", {
  expect_true(is.numeric(doy(vp)))
  expect_true(is.numeric(doy(vpts)))
  expect_true(is.numeric(doy(vpi)))
  expect_true(is.numeric(doy(pvol)))

  expect_true(is.numeric(noy(vp)))
  expect_true(is.numeric(noy(vpts)))
  expect_true(is.numeric(noy(vpi)))
  expect_true(is.numeric(noy(pvol)))
})

test_that("doy_noy() returns correct doy and noy", {
  expect_equal(doy(as.POSIXct("2015-01-01 18:00:00 UTC"), 12.8517, 56.3675), 1)
  expect_equal(noy(as.POSIXct("2015-01-01 18:00:00 UTC"), 12.8517, 56.3675), 2)
})
