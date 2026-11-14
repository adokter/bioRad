
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

test_that("project_as_ppi() preserves its geographic bounding box", {
  data("example_scan")
  range_max <- 10000
  ppi <- project_as_ppi(example_scan, 500, range_max)
  local_crs <- sp::CRS(paste0(
    "+proj=aeqd +lat_0=", example_scan$geo$lat,
    " +lon_0=", example_scan$geo$lon, " +units=m"
  ))
  corners <- sp::SpatialPoints(
    cbind(c(-range_max, range_max), c(-range_max, range_max)),
    proj4string = local_crs
  )
  expected <- sp::bbox(sp::spTransform(
    corners,
    sp::CRS("+proj=longlat +datum=WGS84")
  ))
  rownames(expected) <- c("lon", "lat")

  expect_equal(ppi$geo$bbox, expected, tolerance = 1e-7)

  limited <- project_as_ppi(
    example_scan, 500, range_max,
    xlim = c(12.8, 13), ylim = c(56.3, 56.5)
  )
  expect_equal(unname(limited$geo$bbox["lon", ]), c(12.8, 13))
  expect_equal(unname(limited$geo$bbox["lat", ]), c(56.3, 56.5))
})


test_that("project_as_ppi() accepts a terra SpatRaster as raster argument", {
  data("example_scan")
  b <- project_as_ppi(example_scan, 500, 10000, project = FALSE)
  template <- raster::raster(b$data)
  # a terra SpatRaster template must produce the same ppi as the equivalent RasterLayer
  ppi_spat <- project_as_ppi(example_scan, raster = terra::rast(template), project = FALSE)
  ppi_rast <- project_as_ppi(example_scan, raster = template, project = FALSE)
  expect_s3_class(ppi_spat, "ppi")
  expect_equal(ppi_spat$data, ppi_rast$data, ignore_attr = TRUE)
})

test_that("project_as_ppi works from different projection", {
  data("example_scan")
  expect_s3_class(b <- project_as_ppi(example_scan, 3000, 5000, project = F), "ppi")
  expect_s4_class(r <- raster::rasterFromXYZ(sp::SpatialPoints(sp::spTransform(as(b$data, "SpatialPoints")[s <- c(1, 11), ], "+proj=longlat"), proj4string= sp::CRS("+proj=longlat")), crs = "+proj=longlat"), "RasterLayer")
  expect_s3_class(bb <- project_as_ppi(example_scan, raster = r, project = F), "ppi")
  expect_equal(b$data@data[s, ], bb$data@data[c(1, 4), ], ignore_attr = TRUE)
})
