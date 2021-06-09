# Define non-default options for testing
sd_vvp_thresh <- 3
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
  task_args_quoted <- paste0(
    gsub(",", "',", gsub("=", "='", x$attributes$how$task_args)), "'"
  )
  eval(parse(text= paste0("list(", task_args_quoted, ")")))
}

# Prepare test file (paths)
tmpdir <- tempdir()
pvolfile <- paste(tmpdir, "volume.h5", sep = "/")
pvolfile_out <- paste(tmpdir,"volume_out.h5", sep = "/")
vpfile <- paste(tmpdir, "profile.h5", sep = "/")
# Copy example pvolfile to tempdir/volume.h5
file.copy(
  system.file("extdata", "volume.h5", package = "bioRad"),
  pvolfile,
  overwrite = TRUE
)

docker_ok <- (check_docker(verbose = FALSE) == 0)

test_that("calculate_vp produces a vp", {
  skip_if_no_docker()
  vp <- calculate_vp(file=pvolfile, warnings=FALSE)
  expect_true(inherits(vp,"vp"))
})

test_that("calculate_vp writes vpfile hdf5 output", {
  skip_if_no_docker()
  vp <- calculate_vp(file=pvolfile, vpfile=vpfile, warnings=FALSE)
  expect_true(inherits(vp,"vp"))
})

test_that("calculate_vp writes pvol hdf5 output", {
  skip_if_no_docker()
  vp <- calculate_vp(file=pvolfile, pvolfile_out=pvolfile_out, warnings=FALSE)
  pvol <- read_pvolfile(pvolfile_out)
  expect_true(inherits(vp,"vp"))
  expect_true(inherits(pvol,"pvol"))
  expect_true(inherits(pvol$scans[[1]],"scan"))
  # test that CELL parameter has been added by vol2bird:
  scan <- get_scan(pvol, 0.5)
  expect_true(inherits(scan$params$CELL,"param"))
})

test_that("input arguments are parsed to vol2bird", {
  skip_if_no_docker()

  # run vol2bird with non-default options
  vp <- calculate_vp(file=pvolfile, vpfile=vpfile, sd_vvp_threshold=sd_vvp_thresh,
               rcs=rcs, dual_pol=dual_pol,
               rho_hv=rho_hv, elev_min=elev_min, elev_max=elev_max,
               azim_min=azim_min,azim_max=azim_max,
               range_min=range_min, range_max=range_max,
               n_layer=n_layer, h_layer=h_layer, nyquist_min=nyquist_min,
               warnings=FALSE)

  # check that vol2bird reads non-default options
  expect_equal(as.numeric(task_args(vp)$birdRadarCrossSection),rcs)
  expect_equal(as.logical(as.numeric(task_args(vp)$dualPol)),dual_pol)
  expect_equal(as.numeric(task_args(vp)$rhohvThresMin),rho_hv)
  expect_equal(as.numeric(task_args(vp)$elevMin),elev_min)
  expect_equal(as.numeric(task_args(vp)$elevMax),elev_max)
  expect_equal(as.numeric(task_args(vp)$rangeMin),range_min)
  expect_equal(as.numeric(task_args(vp)$rangeMax),range_max)
  expect_equal(as.numeric(task_args(vp)$nLayers),n_layer)
  expect_equal(as.numeric(task_args(vp)$layerThickness),h_layer)
})

test_that("input arguments are ignored during autoconf", {
  skip_if_no_docker()

  # run vol2bird with non-default options
  expect_true(is.vp(calculate_vp(file=pvolfile, vpfile=vpfile, rcs=rcs, dual_pol=dual_pol,
               rho_hv=rho_hv, elev_min=elev_min, elev_max=elev_max,
               azim_min=azim_min,azim_max=azim_max,
               range_min=range_min, range_max=range_max,
               n_layer=n_layer, h_layer=h_layer, nyquist_min=nyquist_min,
               autoconf=TRUE, warnings=FALSE)))

  # read generated vp
  vp <- read_vpfiles(vpfile)

  # check that vol2bird reads non-default options
  expect_false(as.numeric(task_args(vp)$birdRadarCrossSection)==rcs)
  # ignoring dual_pol check because autoconf-ed
  expect_false(as.numeric(task_args(vp)$rhohvThresMin)==rho_hv)
  expect_false(as.numeric(task_args(vp)$elevMin)==elev_min)
  expect_false(as.numeric(task_args(vp)$elevMax)==elev_max)
  expect_false(as.numeric(task_args(vp)$rangeMin)==range_min)
  expect_false(as.numeric(task_args(vp)$rangeMax)==range_max)
  expect_false(as.numeric(task_args(vp)$nLayers)==n_layer)
  expect_false(as.numeric(task_args(vp)$layerThickness)==h_layer)
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

# TODO / FIXME:
# * arguments not yet tested:
#   - mistnet_elevations, because not stored in h5 currently
#   - dbz_quantity, because test file volume.h5 only has DBZH
#   - local_install, because difficult to test
#   - mount, because difficult to test
# * check if autoconf works (auto-selection of dual_pol, rcs, etc)
#   - requires multiple pvol test files, left for later
