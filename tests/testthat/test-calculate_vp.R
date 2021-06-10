# Define non-default options
sd_vvp_threshold <- 3
rcs <- 200
dual_pol <- TRUE
rho_hv <- 0.8
elev_min <- 1
elev_max <- 5
azim_min <- 10
azim_max <- 350
range_min <- 4000
range_max <- 40000
n_layer <- 30
h_layer <- 150
nyquist_min <- 6

# Create function to extract task_args attributes of vol2bird as a list
task_args <- function(x) {
  task_args_quoted <- paste0(gsub(",", "',", gsub("=", "='", x$attributes$how$task_args)), "'")
  eval(parse(text= paste0("list(", task_args_quoted, ")")))
}

# Prepare test files
tmpdir <- tempdir()
pvolfile <- paste(tmpdir, "pvol.h5", sep = "/")
pvolfile_out <- paste(tmpdir,"pvol_out.h5", sep = "/")
vpfile <- paste(tmpdir, "vp.h5", sep = "/")
file.copy(system.file("extdata", "volume.h5", package = "bioRad"), pvolfile, overwrite = TRUE)

# TODO:
# Arguments not yet tested:
# - mistnet_elevations, because not stored in h5 currently
# - dbz_quantity, because test file volume.h5 only has DBZH
# - mount, because difficult to test
# - local_install, because difficult to test
# - local_mistnet, because difficult to test

})

test_that("calculate_vp() produces a vp object and optional vpfile, pvolfile", {
  skip_if_no_docker()
  vp <- calculate_vp(file = pvolfile, vpfile = vpfile, pvolfile_out = pvolfile_out, warnings = FALSE)
  vp_from_file <- read_vpfiles(vpfile)
  pvol_from_file <- read_pvolfile(pvolfile_out)

  expect_s3_class(vp, "vp")
  expect_s3_class(vp_from_file, "vp")
  expect_s3_class(pvol_from_file, "pvol")
  expect_s3_class(pvol_from_file$scans[[1]], "scan")
  scan <- get_scan(pvol_from_file, 0.5)
  expect_s3_class(scan$params$CELL, "param")
})

test_that("calculate_vp() parses input arguments for vol2bird", {
  skip_if_no_docker()
  # Run with non-default options
  vp <- calculate_vp(file = pvolfile, , warnings = FALSE, sd_vvp_threshold =
                     sd_vvp_threshold, rcs = rcs, dual_pol = dual_pol, rho_hv =
                     rho_hv, elev_min = elev_min, elev_max = elev_max,
                     azim_min = azim_min, azim_max = azim_max, range_min =
                     range_min, range_max = range_max, n_layer = n_layer,
                     h_layer = h_layer, nyquist_min = nyquist_min)
  task_args <- task_args(vp)

  # sd_vvp_threshold is not part of task_args
  expect_equal(rcs, as.numeric(task_args$birdRadarCrossSection))
  expect_equal(dual_pol, as.logical(as.numeric(task_args$dualPol)))
  expect_equal(rho_hv, as.numeric(task_args$rhohvThresMin))
  expect_equal(elev_min, as.numeric(task_args$elevMin))
  expect_equal(elev_max, as.numeric(task_args$elevMax))
  expect_equal(azim_min, as.numeric(task_args$azimMin))
  expect_equal(azim_max, as.numeric(task_args$azimMax))
  expect_equal(range_min, as.numeric(task_args$rangeMin))
  expect_equal(range_max, as.numeric(task_args$rangeMax))
  expect_equal(n_layer, as.numeric(task_args$nLayers))
  expect_equal(h_layer, as.numeric(task_args$layerThickness))
  expect_equal(nyquist_min, as.numeric(task_args$minNyquist))
})

test_that("calculate_vp() ignores input arguments if autoconf", {
  skip_if_no_docker()
  # Run with non-default options
  vp <- calculate_vp(file = pvolfile, , warnings = FALSE, sd_vvp_threshold =
                     sd_vvp_threshold, rcs = rcs, dual_pol = dual_pol, rho_hv =
                     rho_hv, elev_min = elev_min, elev_max = elev_max,
                     azim_min = azim_min, azim_max = azim_max, range_min =
                     range_min, range_max = range_max, n_layer = n_layer,
                     h_layer = h_layer, nyquist_min = nyquist_min,
                     autoconf = TRUE)
  task_args <- task_args(vp)

  # sd_vvp_threshold is not part of task_args
  expect_false(rcs == as.numeric(task_args$birdRadarCrossSection))
  expect_true(as.logical(as.numeric(task_args$dualPol))) # Set to true by autoconf
  expect_false(rho_hv == as.numeric(task_args$rhohvThresMin))
  expect_false(elev_min == as.numeric(task_args$elevMin))
  expect_false(elev_max == as.numeric(task_args$elevMax))
  expect_false(azim_min == as.numeric(task_args$azimMin))
  expect_false(azim_max == as.numeric(task_args$azimMax))
  expect_false(range_min == as.numeric(task_args$rangeMin))
  expect_false(range_max == as.numeric(task_args$rangeMax))
  expect_false(n_layer == as.numeric(task_args$nLayers))
  expect_false(h_layer == as.numeric(task_args$layerThickness))
  expect_false(nyquist_min == as.numeric(task_args$minNyquist))
})

test_that("MistNet is working", {
  skip_if_no_docker()

  # run vol2bird with non-default options
  vp <- calculate_vp(file=pvolfile, vpfile=vpfile,warnings=FALSE,
                     mistnet = TRUE, pvolfile_out = pvolfile_out)

  pvol <- read_pvolfile(pvolfile_out)

  # get one of the mistnet elevations (1.5 degree)
  scan <- get_scan(pvol, 0.5)

  expect_true(inherits(vp,"vp"))
  expect_true(inherits(scan,"scan"))
  expect_true(inherits(pvol,"pvol"))

  # check that mistnet param WEATHER is added
  expect_true(inherits(scan$params$WEATHER,"param"))
})

test_that("dealiasing can be suppressed", {
  skip_if_no_docker()

  # run vol2bird without dealiasing
  vp <- calculate_vp(file=pvolfile, vpfile=vpfile, warnings=FALSE,
                     nyquist_min=nyquist_min, dealias=F)
  expect_equal(vp$attributes$how$dealiased,0)
  # the dealias attribute is stored at two locations, this one is the duplicate
  # but including to guarantee overall consistency.
  expect_equal(task_args(vp)$dealiasVrad,"0")

  # run vol2bird with dealiasing
  vp <- calculate_vp(file=pvolfile, vpfile=vpfile, warnings=FALSE,
                     nyquist_min=nyquist_min, dealias=T)
  expect_equal(vp$attributes$how$dealiased,1)
  # the dealias attribute is stored at two locations, this one is the duplicate
  # but including to guarantee overall consistency.
  expect_equal(task_args(vp)$dealiasVrad,"1")
})
