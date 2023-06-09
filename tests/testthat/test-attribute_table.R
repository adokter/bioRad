pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
example_pvol <- read_pvolfile(pvolfile)
data(example_scan)
test_that("returns error on incorrect parameters", {
  expect_error(
    attribute_table("not_a_vp"),
    regexp = "`x` must be a pvol or scan object",
    fixed = TRUE
    )
  expect_error(
    attribute_table(example_pvol, select = 1L),
    regexp = "when provided, `select` must be a character vector",
    fixed = TRUE
    )
})
test_that("result is correct", {
  expect_equal(nrow(attribute_table(example_pvol)), length(example_pvol$scans))
  expect_equal(attribute_table(example_pvol)$param[[2]], names(example_pvol$scans[[2]]$param))
  expect_equal(attribute_table(example_pvol)$where.elangle, get_elevation_angles(example_pvol))
  expect_equal(attribute_table(example_pvol)$where.nbins[[3]], nrow(example_pvol$scans[[3]]$param[[1]]))
  expect_equal(attribute_table(example_scan), attribute_table(example_pvol)[1, ])
  expect_equal(attribute_table(example_pvol, select = "how.stopazT", F)[, 1], lapply(example_pvol$scans, function(x) x$attributes$how$stopazT))
})
test_that("add_params works", {
  expect_true("param" %in% colnames(attribute_table(example_pvol)))
  expect_type(attribute_table(example_pvol)$param, "list")
  expect_type(attribute_table(example_pvol)$param[[2]], "character")
})
test_that("select argument works", {
  expect_equal(setdiff(colnames(attribute_table(example_scan)), c(
    "how.lowprf",
    "how.midprf",
    "how.highprf",
    "where.elangle",
    "where.nbins",
    "where.nrays",
    "where.rscale",
    "how.NI",
    "param"
  )), character(0))
  expect_equal(colnames(attribute_table(example_pvol, select = NULL)), c(names(unlist(example_pvol$scans[[1]]$attributes, recursive = F)), 'param'))
})

test_that("silently ignores non existant colums", {
  expect_equal(setdiff(colnames(attribute_table(example_scan, select = c(
    "how.lowprf",
    "how.NI", "asdf"
  ))), c(
    "how.lowprf",
    "how.NI",
    "param"
  )), character(0))
  expect_silent(attribute_table(example_scan, select = c(
    "how.lowprf",
    "how.NI", "asdf"
  )))
})
