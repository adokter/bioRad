pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
example_pvol <- read_pvolfile(pvolfile)
example_pvol_calc <- calculate_param(example_pvol, D = 2*DBZH)

data(example_scan)
example_scan_calc <- calculate_param(example_scan, D = 2*DBZH)

ppi <- project_as_ppi(example_scan)
ppi_calc <- calculate_param(ppi, D = 2*DBZH)


test_that("calculate_param() returns error on incorrect parameters", {
  expect_error(calculate_param("Not_a_pvol/ppi/scan"))
})


test_that("calculate_param() returns object of the correct class", {
  expect_equal(class(example_pvol), class(example_pvol_calc))
  expect_equal(class(example_scan), class(example_scan_calc))
  expect_equal(class(ppi), class(ppi_calc))
})


test_that("calculate_param() returns object of the correct size", {
  expect_equal(length(example_pvol), length(example_pvol_calc))
  expect_equal(length(example_scan), length(example_scan_calc))
  expect_equal(length(ppi), length(ppi_calc))
})


test_that("calculate_param() adds calculated parameter", {
  expect_equal(example_pvol_calc$scans[[1]]$params$DBZH[1:10]*2,
               example_pvol_calc$scans[[1]]$params$D[1:10])
  expect_equal(example_scan_calc$params$DBZH[1:10]*2,
               example_scan_calc$params$D[1:10])
  expect_equal(ppi_calc$data$DBZH*2, ppi_calc$data$D)
})
