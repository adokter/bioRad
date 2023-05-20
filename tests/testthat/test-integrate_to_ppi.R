test_that("integrate_to_ppi() returns error on incorrect parameters", {

})

test_that("integrate_to_ppi() raster argument produces expected output", {
  data(example_vp)
  expect_true(file.exists(pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")))
  expect_s3_class(example_pvol <- read_pvolfile(pvolfile), "pvol")
  expect_s3_class(my_ppi <- integrate_to_ppi(example_pvol, example_vp, nx = 60, ny = 50), "ppi")
  expect_equal(
    raster::raster(integrate_to_ppi(example_pvol, example_vp, raster = raster::raster(my_ppi$data))$data),
    raster::raster(my_ppi$data)
  )
})

test_that("integrate_to_ppi() produces simular output when limits are set", {
  data(example_vp)
  expect_true(file.exists(pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")))
  expect_s3_class(example_pvol <- read_pvolfile(pvolfile), "pvol")
  expect_s3_class(my_ppi <- integrate_to_ppi(example_pvol,
                                             example_vp,
                                             xlim = c(-10010, 10000), ylim = c(-11010, 10000), res = 410
  ), "ppi")
  expect_equal(
    raster::raster(integrate_to_ppi(example_pvol, example_vp, raster = raster::raster(my_ppi$data))$data),
    raster::raster(my_ppi$data)
  )
})

test_that("integrate_to_ppi() produces same values on cropped raster", {
  data(example_vp)
  expect_true(file.exists(pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")))
  expect_s3_class(example_pvol <- read_pvolfile(pvolfile), "pvol")
  expect_s3_class(my_ppi <- integrate_to_ppi(example_pvol,
                                             example_vp,
                                             xlim = c(-10010, 10000), ylim = c(-11010, 10000), res = 210
  ), "ppi")
  expect_equal(
    raster::values(raster::raster(integrate_to_ppi(example_pvol, example_vp,
                                   raster = raster::crop(raster::raster(my_ppi$data), raster::extent(0, 3000, 1000, 5000))
    )$data)),
    raster::values(raster::crop(raster::raster(my_ppi$data), raster::extent(0, 3000, 1000, 5000)))
  )
})

test_that("check if other projection gives same result", {
  data(example_vp)
  expect_true(file.exists(pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")))
  expect_s3_class(example_pvol <- read_pvolfile(pvolfile), "pvol")
  expect_s3_class(my_ppi <- integrate_to_ppi(example_pvol,
                                             example_vp,
                                             xlim = c(-10010, 10000), ylim = c(-11010, 10000), res = 510
  ), "ppi")
  expect_s4_class(my_raster <- raster::rasterFromXYZ(spTransform(methods::as(my_ppi$data, "SpatialPointsDataFrame"), "+proj=longlat")[c(3, 7), ]), "RasterLayer")
  expect_silent(proj4string(my_raster) <- "+proj=longlat")
  expect_equal(
    raster::values(raster::raster(integrate_to_ppi(example_pvol, example_vp, raster = my_raster)$data))[!is.na(raster::values(my_raster))],
    raster::values(my_raster)[!is.na(raster::values(my_raster))]
  )
})
