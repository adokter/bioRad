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
  expect_error(
    composite_ppi(ppis[2:3], xlim = c(9,NA)),
    regexp = paste("'xlim' should be a vector with two numeric values",
                     "for lower and upper bound respectively"),
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[2:3], ylim = "a"),
    regexp = "'ylim' should be a numeric vector of length two",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[2:3], ylim = c(1,9,4)),
    regexp = "'ylim' should be a numeric vector of length two",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[2:3], ylim = c(9,NA)),
    regexp = paste("'ylim' should be a vector with two numeric values",
                   "for lower and upper bound respectively"),
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], res = seq(3)),
    regexp = "length(res) not less than or equal to 2",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], res = c("a",2)),
    regexp = "res is not a numeric or integer vector",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], crs = "a"),
    regexp = "invalid crs: a",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], method = "not_a_method"),
    regexp = "'method' should be one or multiple of 'max', 'mean', 'min' or 'idw'",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], param = "all", method = c("max","min","mean")),
    regexp = "'method' should be of length 1 or length(param)",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], param = "DBZH", method = c("max","min")),
    regexp = "'method' should be of length 1 or length(param)",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2],
                  param = c("DBZH","ZDR"),
                  method = c("max","min","mean")),
    regexp = "'method' should be of length 1 or length(param)",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], coverage = 1),
    regexp = "coverage is not a flag (a length one logical vector).",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], coverage = c(TRUE,TRUE)),
    regexp = "coverage is not a flag (a length one logical vector).",
    fixed = TRUE
  )
  expect_error(
    composite_ppi(ppis[1:2], coverage = NA),
    regexp = "missing value where TRUE/FALSE needed",
    fixed = TRUE
  )
})
