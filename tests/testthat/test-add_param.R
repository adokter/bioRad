# Load the polar volume that ships with bioRad as example data
pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

# Build a raster matching the topology of the lowest scan and fill it with
# synthetic data. scan_to_raster() returns a raster in the radar's local
# azimuthal-equidistant projection
make_synthetic_raster <- function(scan) {
  template <- scan_to_raster(scan, nx = 50, ny = 50, param = "DBZH")
  r <- raster::raster(template)
  # synthetic, fully populated values so extraction within the extent is non-NA
  raster::values(r) <- seq_len(raster::ncell(r))
  names(r) <- "synthetic"
  r
}

lowest_scan <- get_scan(pvol, 0.5)
# Both raster classes are tested as input: an in-memory RasterLayer and a terra
# SpatRaster. add_param() must accept either, and the two cover both branches of
# the RasterLayer-to-SpatRaster conversion in add_param.scan()/add_param.pvol().
synthetic_raster <- make_synthetic_raster(lowest_scan)
synthetic_spatraster <- terra::rast(synthetic_raster)

test_that("add_param() requires its arguments", {
  # `param` has no default and must be supplied
  expect_error(
    add_param(lowest_scan, synthetic_raster),
    regexp = "missing subscript",
    fixed = TRUE
  )
  # the generic only dispatches for `scan` and `pvol` objects
  expect_error(
    add_param("not_a_scan_or_pvol", synthetic_spatraster, "synthetic"),
    regexp = "no applicable method for 'add_param'"
  )
})

test_that("add_param.scan() errors when x is not a scan", {
  expect_error(
    add_param.scan("not_a_scan", synthetic_raster, "synthetic"),
    regexp = 'inherits(x, "scan") is not TRUE',
    fixed = TRUE
  )
})

test_that("add_param() adds a parameter to a scan (in-memory RasterLayer input)", {
  scan_out <- add_param(lowest_scan, synthetic_raster, "synthetic")

  # the returned object is still a scan
  expect_s3_class(scan_out, "scan")
  # the new parameter has been added under the requested name
  expect_true("synthetic" %in% names(scan_out$params))
  # the added parameter is a proper scan parameter
  expect_s3_class(scan_out$params[["synthetic"]], "param")
  # it shares the dimensions of the existing scan parameters
  expect_equal(
    dim(scan_out$params[["synthetic"]]),
    dim(scan_out$params[[1]])
  )
  # the `param` attribute is set to the supplied name
  expect_equal(attributes(scan_out$params[["synthetic"]])$param, "synthetic")
  # at least some range gates fall within the raster and receive a value
  expect_false(all(is.na(scan_out$params[["synthetic"]])))
})

test_that("add_param() honours the param argument name (SpatRaster input)", {
  scan_out <- add_param(lowest_scan, synthetic_spatraster, "HGHT")
  expect_s3_class(scan_out, "scan")
  expect_true("HGHT" %in% names(scan_out$params))
  expect_equal(attributes(scan_out$params[["HGHT"]])$param, "HGHT")
})

test_that("add_param() adds a parameter to every scan of a pvol (RasterLayer input)", {
  expect_s4_class(synthetic_raster, "RasterLayer")

  pvol_out <- add_param(pvol, synthetic_raster, "synthetic")

  # the returned object is still a pvol
  expect_s3_class(pvol_out, "pvol")
  # every scan in the volume now carries the new parameter
  expect_true(all(vapply(
    pvol_out$scans,
    function(s) "synthetic" %in% names(s$params),
    logical(1)
  )))
  expect_equal(
    attributes(get_scan(pvol_out, 0.5)$params[["synthetic"]])$param,
    "synthetic"
  )
})

test_that("add_param() accepts a terra SpatRaster for a pvol", {
  expect_s4_class(synthetic_spatraster, "SpatRaster")

  pvol_out <- add_param(pvol, synthetic_spatraster, "synthetic")

  expect_s3_class(pvol_out, "pvol")
  expect_true(all(vapply(
    pvol_out$scans,
    function(s) "synthetic" %in% names(s$params),
    logical(1)
  )))
})
