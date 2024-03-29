
remove_bboxlatlon <- function(x) { attributes(x)$bboxlatlon <- NULL; return(x) }

test_that("returns error on incorrect parameters", {
  # I don't think a user could naturally reach these errors:
  expect_error(
    project_as_ppi.param(example_scan),
    regexp = 'inherits(x, "param") is not TRUE',
    fixed = TRUE
  )
  expect_error(
    project_as_ppi.scan(get_param(example_scan, "DBZH")),
    regexp = 'inherits(x, "scan") is not TRUE',
    fixed = TRUE
  )
})

test_that("sample_polar works", {
  data("example_scan")
  a <- example_scan$params[[3]]
  expect_s4_class(b <- bioRad:::sample_polar(example_scan$params[[3]], 500, 10000, xlim = 12.9 + c(-1, 1), ylim = 56.4 + c(-1, 1), project = F), "SpatialGridDataFrame")
  expect_equal(remove_bboxlatlon(b), bioRad:::sample_polar(example_scan$params[[3]], raster::raster(b), project = F), ignore_attr = TRUE)
  expect_s4_class(bb <- bioRad:::sample_polar(example_scan$params[[3]], 5000, 10000, xlim = 12.9 + c(-1, 1), ylim = 56.4 + c(-1, 1), project = T), "SpatialGridDataFrame")
  expect_equal(remove_bboxlatlon(bb), bioRad:::sample_polar(example_scan$params[[3]], raster::raster(bb), project = T), ignore_attr = TRUE)
})

test_that("project_as_ppi works", {
  data("example_scan")
  expect_s3_class(b <- project_as_ppi(example_scan, 500, 10000, project = F), "ppi")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = raster::raster(b$data), project = F), "ppi")
  expect_equal(bb$radar, b$radar, ignore_attr = TRUE)
  expect_equal(bb$datetime, b$datetime, ignore_attr = TRUE)
  expect_equal(bb$data, remove_bboxlatlon(b$data), ignore_attr = TRUE)
  expect_s3_class(b <- project_as_ppi(example_scan, 50, 1000, project = T), "ppi")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = raster::raster(b$data), project = T), "ppi")
  expect_equal(bb$radar, b$radar, ignore_attr = TRUE)
  expect_equal(bb$datetime, b$datetime, ignore_attr = TRUE)
  expect_equal(bb$data, remove_bboxlatlon(b$data), ignore_attr = TRUE)
})


test_that("project_as_ppi works from different projection", {
  data("example_scan")
  expect_s3_class(b <- project_as_ppi(example_scan, 3000, 5000, project = F), "ppi")
  expect_s4_class(r <- raster::rasterFromXYZ(sp::SpatialPoints(sp::spTransform(as(b$data, "SpatialPoints")[s <- c(1, 11), ], "+proj=longlat"), proj4string= sp::CRS("+proj=longlat")), crs = "+proj=longlat"), "RasterLayer")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = r, project = F), "ppi")
  expect_equal(b$data@data[s, ], bb$data@data[c(1, 4), ], ignore_attr = TRUE)
})
