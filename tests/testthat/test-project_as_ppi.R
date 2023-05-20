test_that("returns error on incorrect parameters", {

})

#context("check raster input for project_as_ppi")
test_that("sample_polar works", {
  data("example_scan")
  a <- example_scan$params[[3]]
  expect_s4_class(b <- bioRad:::sample_polar(example_scan$params[[3]], 500, 10000, xlim = 12.9 + c(-1, 1), ylim = 56.4 + c(-1, 1), project = F), "SpatialGridDataFrame")
  expect_equivalent(b, bioRad:::sample_polar(example_scan$params[[3]], raster(b), project = F))
  expect_s4_class(bb <- bioRad:::sample_polar(example_scan$params[[3]], 5000, 10000, xlim = 12.9 + c(-1, 1), ylim = 56.4 + c(-1, 1), project = T), "SpatialGridDataFrame")
  expect_equivalent(bb, bioRad:::sample_polar(example_scan$params[[3]], raster(bb), project = T))
})

test_that("project_as_ppi works", {
  data("example_scan")
  expect_s3_class(b <- project_as_ppi(example_scan, 500, 10000, project = F), "ppi")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = raster(b$data), project = F), "ppi")
  expect_equal(bb$radar, b$radar)
  expect_equal(bb$datetime, b$datetime)
  expect_equivalent(bb$data, b$data)
  expect_s3_class(b <- project_as_ppi(example_scan, 50, 1000, project = T), "ppi")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = raster(b$data), project = T), "ppi")
  expect_equal(bb$radar, b$radar)
  expect_equal(bb$datetime, b$datetime)
  expect_equivalent(bb$data, b$data)
})


test_that("project_as_ppi works from different projection", {
  data("example_scan")
  expect_s3_class(b <- project_as_ppi(example_scan, 3000, 5000, project = F), "ppi")
  expect_s4_class(r <- raster::rasterFromXYZ(spTransform(methods::as(b$data, "SpatialPoints")[s <- c(1, 11), ], "+proj=longlat"), crs = "+proj=longlat"), "RasterLayer")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = r, project = F), "ppi")
  expect_equivalent(b$data@data[s, ], bb$data@data[c(1, 4), ])
})
