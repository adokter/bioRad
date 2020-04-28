test_that("s... returns error on incorrect parameters", {

})
test_that("scan_to_raster() raster argument produces expected raster output", {
  data(example_vpts)
  expect_s4_class(b <- scan_to_raster(example_scan, ylim = c(55, 57), xlim = c(12, 13), res = .1), "RasterBrick")
  expect_equal(b, scan_to_raster(example_scan, raster = raster(b)))
})
