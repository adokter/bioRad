test_that("integrate_to_ppi() returns error on incorrect parameters", {
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  example_pvol <- read_pvolfile(pvolfile)
  data(example_vp)

  expect_error(
    integrate_to_ppi("not_a_pvol"),
    regexp = "'pvol' should be an object of class pvol",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol),
    regexp = 'argument "vp" is missing, with no default',
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, vp = "not_a_vp"),
    regexp = "'vp' should be an object of class vp",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      vp = example_vp,
      nx = "a"
    ),
    regexp = "'nx' should be an integer",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      vp = example_vp,
      ny = 1:3
    ),
    regexp = "'ny' should be an integer",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      xlim = "not_a_limit"
    ),
    regexp = "'xlim' should be an integer vector of length two",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      xlim = c(0.5, 14.9, 47)
    ),
    regexp = "'xlim' should be an integer vector of length two",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      xlim = c(42, NA)
    ),
    regexp = "'xlim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      xlim = c(42E42, 42)
    ),
    regexp = "'xlim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      xlim = "not_a_limit"
    ),
    regexp = "'xlim' should be an integer vector of length two",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      ylim = c(0.5, 14.9, 47)
    ),
    regexp = "'ylim' should be an integer vector of length two",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      ylim = c(42, NA)
    ),
    regexp = "'ylim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      ylim = c(42E42, 42)
    ),
    regexp = "'ylim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      zlim = "not_a_limit"
    ),
    regexp = "'zlim' should be an integer vector of length two",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      zlim = c(0.5, 14.9, 47)
    ),
    regexp = "'zlim' should be an integer vector of length two",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      zlim = c(42, NA)
    ),
    regexp = "'zlim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol,
      example_vp,
      zlim = c(42E42, 42)
    ),
    regexp = "'zlim' should be a vector with two numeric values for upper and lower bound",
    fixed = TRUE
  )

  example_pvol_no_lat <- example_pvol
  example_pvol_no_lat$geo$lat <- NULL
  expect_error(
    integrate_to_ppi(
      example_pvol_no_lat,
      example_vp
    ),
    regexp = "radar latitude cannot be found in polar volume"
  )
  example_pvol_no_lon <- example_pvol
  example_pvol_no_lon$geo$lon <- NULL
  expect_error(
    integrate_to_ppi(
      example_pvol_no_lon,
      example_vp
    ),
    regexp = "radar longitude cannot be found in polar volume"
  )
  example_pvol_no_height <- example_pvol
  example_pvol_no_height$geo$height <- NULL
  expect_error(
    integrate_to_ppi(
      example_pvol_no_height,
      example_vp
    ),
    regexp = "antenna height cannot be found in polar volume"
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, param = "NOT_A_PARAM"),
    regexp = "param `NOT_A_PARAM` not one of DBZH, DBZV, DBZ, TH or TV",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, param = c("NOT_A_PARAM", "eta_sum")),
    regexp = "param `NOT_A_PARAM` & `eta_sum` not one of DBZH, DBZV, DBZ, TH or TV",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, param = c("NOT_A_PARAM", "TV")),
    regexp = "param `NOT_A_PARAM` not one of DBZH, DBZV, DBZ, TH or TV",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, quantity = "not_a_quantity"),
    regexp = "quantity 'not_a_quantity' not one of 'eta' or 'dens'",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, quantity = c("dens", "eta")),
    regexp = "length(quantity) not equal to 1",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, param_ppi = "not_a_param!"),
    regexp = "unknown param_ppi",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, k = "a"),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, k = seq(9)),
    regexp = "k is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, re = "a"),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, re = seq(9)),
    regexp = "re is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, rp = "a"),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    integrate_to_ppi(example_pvol, example_vp, rp = seq(9)),
    regexp = "rp is not a number (a length one numeric vector).",
    fixed = TRUE
  )

  example_pvol_no_DBZH <- example_pvol
  example_pvol_no_DBZH$scans[[2]]$params$DBZH <- NULL
  expect_warning(
    integrate_to_ppi(
      example_pvol_no_DBZH,
      example_vp
    ),
    regexp = "ignoring scan(s) 2 because they have no scan parameter DBZH",
    fixed = TRUE
  )
  example_pvol_no_DBZH$scans[[1]]$params$DBZH <- NULL
  example_pvol_no_DBZH$scans[[3]]$params$DBZH <- NULL
  expect_error(
    integrate_to_ppi(
      example_pvol_no_DBZH,
      example_vp
    ),
    regexp = "polar volume contains no scans with scan parameter 'DBZH'",
    fixed = TRUE
  )
  # expect_error(
  #   integrate_to_ppi(example_pvol,
  #                    vp = example_vp,
  #                    ny = 3,
  #                    res = c(2000,2000)),
  #   regexp = "'ny' should be an integer",
  #   fixed = TRUE
  # )
})

test_that("add_expected_eta_to_scan() returns error on incorrect parameters", {
  scan_no_height <- example_scan
  scan_no_height$geo$height <- NULL
  expect_error(
    add_expected_eta_to_scan(
      scan_no_height,
      example_vp
    ),
    regexp = "antenna height cannot be found in scan, specify antenna height"
  )
  expect_error(
    add_expected_eta_to_scan(example_scan,
      example_vp,
      quantity = "not_a_quantity"
    ),
    regexp = "quantity 'not_a_quantity' not one of 'eta' or 'dens'",
    fixed = TRUE
  )
  expect_error(
    add_expected_eta_to_scan(example_scan,
      example_vp,
      param = "not_a_param"
    ),
    regexp = "not_a_param not one of DBZH, DBZV, DBZ, TH, TV",
    fixed = TRUE
  )

  scan_no_lat <- example_scan
  scan_no_lat$geo$lat <- NULL
  expect_error(
    add_expected_eta_to_scan(example_scan,
      example_vp,
      lat = "a"
    ),
    regexp = "lat is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    add_expected_eta_to_scan(
      scan_no_lat,
      example_vp
    ),
    regexp = "radar latitude cannot be found in polar volume, specify using 'lat' argument",
    fixed = TRUE
  )


  scan_no_lon <- example_scan
  scan_no_lon$geo$lon <- NULL
  expect_error(
    add_expected_eta_to_scan(example_scan,
                             example_vp,
                             lon = "a"
    ),
    regexp = "lon is not a number (a length one numeric vector).",
    fixed = TRUE
  )
  expect_error(
    add_expected_eta_to_scan(
      scan_no_lon,
      example_vp
    ),
    regexp = "radar longitude cannot be found in polar volume, specify using 'lon' argument",
    fixed = TRUE
  )
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
  expect_s4_class(my_raster <- raster::rasterFromXYZ(sp::spTransform(methods::as(my_ppi$data, "SpatialPointsDataFrame"), "+proj=longlat")[c(3, 7), ]), "RasterLayer")
  expect_silent(sp::proj4string(my_raster) <- "+proj=longlat")
  expect_equal(
    raster::values(raster::raster(integrate_to_ppi(example_pvol, example_vp, raster = my_raster)$data))[!is.na(raster::values(my_raster))],
    raster::values(my_raster)[!is.na(raster::values(my_raster))]
  )
})
