
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
  expect_true(terra::compareGeom(bb$data, b$data, stopOnError = FALSE))
  expect_equal(terra::values(bb$data), terra::values(b$data), ignore_attr = TRUE)
  expect_s3_class(b <- project_as_ppi(example_scan, 50, 1000, project = T), "ppi")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = raster::raster(b$data), project = T), "ppi")
  expect_equal(bb$radar, b$radar, ignore_attr = TRUE)
  expect_equal(bb$datetime, b$datetime, ignore_attr = TRUE)
  expect_true(terra::compareGeom(bb$data, b$data, stopOnError = FALSE))
  expect_equal(terra::values(bb$data), terra::values(b$data), ignore_attr = TRUE)
})

test_that("project_as_ppi preserves projected values and geometry", {
  legacy <- bioRad:::sample_polar(
    example_scan$params[[1]], 500, 10000, project = FALSE,
    ylim = NULL, xlim = NULL
  )
  ppi <- project_as_ppi(example_scan$params[[1]], 500, 10000, project = FALSE)

  expect_s4_class(ppi$data, "SpatRaster")
  expect_equal(terra::values(ppi$data)[, 1], legacy@data[, 1])
  expect_equal(
    unname(as.vector(terra::ext(ppi$data))),
    unname(c(legacy@bbox[1, ], legacy@bbox[2, ]))
  )
  expect_true(sf::st_crs(ppi$data) == sf::st_crs(legacy))
})


test_that("project_as_ppi() accepts a terra SpatRaster as raster argument", {
  data("example_scan")
  b <- project_as_ppi(example_scan, 500, 10000, project = FALSE)
  template <- raster::raster(b$data)
  # a terra SpatRaster template must produce the same ppi as the equivalent RasterLayer
  ppi_spat <- project_as_ppi(example_scan, raster = terra::rast(template), project = FALSE)
  ppi_rast <- project_as_ppi(example_scan, raster = template, project = FALSE)
  expect_s3_class(ppi_spat, "ppi")
  expect_true(terra::compareGeom(ppi_spat$data, ppi_rast$data, stopOnError = FALSE))
  expect_equal(terra::values(ppi_spat$data), terra::values(ppi_rast$data), ignore_attr = TRUE)
})

test_that("project_as_ppi works from different projection", {
  data("example_scan")
  expect_s3_class(b <- project_as_ppi(example_scan, 3000, 5000, project = F), "ppi")
  legacy_grid <- bioRad:::sample_polar(
    example_scan$params[[1]], 3000, 5000, project = F,
    ylim = NULL, xlim = NULL
  )
  expect_s4_class(r <- raster::rasterFromXYZ(sp::SpatialPoints(sp::spTransform(as(legacy_grid, "SpatialPoints")[s <- c(1, 11), ], "+proj=longlat"), proj4string= sp::CRS("+proj=longlat")), crs = "+proj=longlat"), "RasterLayer")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = r, project = F), "ppi")
  expect_equal(terra::values(b$data)[s, ], terra::values(bb$data)[c(1, 4), ], ignore_attr = TRUE)
})
