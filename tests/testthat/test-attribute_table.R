pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
example_pvol <- read_pvolfile(pvolfile)
data(example_scan)
test_that("returns error on incorrect parameters", {
  expect_error(attribute_table("not_a_vp"))
  expect_error(attribute_table(example_pvol, add_param = "a"))
  expect_error(attribute_table(example_pvol, select = 1L))
})
test_that("result is correct", {
  expect_equal(nrow(attribute_table(example_pvol)), length(example_pvol$scans))
  expect_equal(attribute_table(example_pvol)$params[[2]], names(example_pvol$scans[[2]]$params))
  expect_equal(attribute_table(example_pvol)$where.elangle, get_elevation_angles(example_pvol))
  expect_equal(attribute_table(example_pvol)$where.nbins[[3]], nrow(example_pvol$scans[[3]]$params[[1]]))
  expect_equal(attribute_table(example_scan), attribute_table(example_pvol)[1, ])
  expect_equal(attribute_table(example_pvol, select = "how.stopazT", F)[, 1], lapply(example_pvol$scans, function(x) x$attributes$how$stopazT))
})
test_that("add_params works", {
  expect_false("params" %in% colnames(attribute_table(example_pvol, add_params = F)))
  expect_true("params" %in% colnames(attribute_table(example_pvol, add_params = T)))
  expect_true("params" %in% colnames(attribute_table(example_pvol)))
  expect_type(attribute_table(example_pvol)$params, "list")
  expect_type(attribute_table(example_pvol)$params[[2]], "character")
})
test_that("select argument works", {
  expect_equal(setdiff(colnames(attribute_table(example_scan, add_params = F)), c(
    "how.lowprf",
    "how.midprf",
    "how.highprf",
    "where.elangle",
    "where.nbins",
    "where.nrays",
    "where.rscale",
    "how.NI"
  )), character(0))
  expect_equal(colnames(attribute_table(example_pvol, select = NULL, add_params = F)), names(unlist(example_pvol$scans[[1]]$attributes, recursive = F)))
})

test_that("silently isnores non existant colums", {
  expect_equal(setdiff(colnames(attribute_table(example_scan, select = c(
    "how.lowprf",
    "how.NI", "asdf"
  ), add_params = F)), c(
    "how.lowprf",
    "how.NI"
  )), character(0))
  expect_silent(attribute_table(example_scan, select = c(
    "how.lowprf",
    "how.NI", "asdf"
  )))
})
