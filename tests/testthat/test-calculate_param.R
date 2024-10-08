example_pvol <- read_pvolfile(
  system.file("extdata", "volume.h5", package = "bioRad")
)
ppi <- project_as_ppi(example_scan)

# Create pvol, scan, ppi with new_param = DBZH * 2
example_pvol_calc <- calculate_param(example_pvol, new_param = DBZH * 2)
example_scan_calc <- calculate_param(example_scan, new_param = DBZH * 2)
ppi_calc <- calculate_param(ppi, new_param = DBZH * 2)

test_that("calculate_param() returns error on incorrect parameters", {
  expect_error(calculate_param("not_a_pvol/ppi/scan"),
               regexp = 'no applicable method for \'calculate_param\' applied to an object of class "character"',
               fixed = TRUE)
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

test_that("calculate_param() adds the calculated parameter", {
  expect_equal(
    example_pvol_calc$scans[[1]]$params$DBZH[1:10] * 2,
    example_pvol_calc$scans[[1]]$params$new_param[1:10]
  )
  expect_equal(
    example_scan_calc$params$DBZH[1:10] * 2,
    example_scan_calc$params$new_param[1:10]
  )
  expect_equal(ppi_calc$data$DBZH * 2, ppi_calc$data$new_param)
})
test_that("calculate_param() works with if_else", {
  skip_if_not_installed('dplyr')
  require(dplyr)
  expect_equal(
    calculate_param(get_scan(example_pvol, 0.5), DBZH=if_else(c(RHOHV)>.95, NA, c(DBZH)) ),
    get_scan(calculate_param(example_pvol, DBZH=if_else(c(RHOHV)>.95, NA, c(DBZH)) ), .5)
  )
  expect_equal(
    calculate_param(example_scan, DBZH=dplyr::if_else(c(RHOHV)>.95, NA, c(DBZH)) ),
    calculate_param(example_scan, DBZH=if_else(c(RHOHV)>.95, NA, c(DBZH)) )
  )
})
