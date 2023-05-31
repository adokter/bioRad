test_that("composite_ppi() returns error on incorrect parameters", {
  pvol <- read_pvolfile(system.file("extdata", "volume.h5", package = "bioRad"))
  ppis <- lapply(pvol$scans, project_as_ppi)
  expect_error(
    composite_ppi(example_vp),
    regexp = "'composite' expects objects of class ppi only",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[2:3], nx = "a"),
    regexp = "'nx' should be an integer",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[2:3], ny = "a"),
    regexp = "'ny' should be an integer",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[2:3], xlim = "a"),
    regexp = "'xlim' should be a numeric vector of length two",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[2:3], xlim = c(1,9,4)),
    regexp = "'xlim' should be a numeric vector of length two",
    fixed = TRUE
  )
})
