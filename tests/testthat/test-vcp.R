pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
example_pvol <- read_pvolfile(pvolfile)
data(example_scan)
test_that("returns error on incorrect parameters", {
  expect_error(vcp("not_a_vp"))
  expect_error(vcp(example_pvol, add_param = "a"))
  expect_error(vcp(example_pvol, select = 1L))
})
test_that("result is correct", {
  expect_equal(nrow(vcp(example_pvol)), length(example_pvol$scans))
  expect_equal(vcp(example_pvol)$params[[2]], names(example_pvol$scans[[2]]$params))
  expect_equal(vcp(example_pvol)$where.elangle, get_elevation_angles(example_pvol))
  expect_equal(vcp(example_pvol)$where.nbins[[3]], nrow(example_pvol$scans[[3]]$params[[1]]))
  expect_equal(vcp(example_scan), vcp(example_pvol)[1, ])
  expect_equal(vcp(example_pvol, select = "how.stopazT", F)[, 1], lapply(example_pvol$scans, function(x) x$attributes$how$stopazT))
})
test_that("add_params works", {
  expect_false("params" %in% colnames(vcp(example_pvol, add_params = F)))
  expect_true("params" %in% colnames(vcp(example_pvol, add_params = T)))
  expect_true("params" %in% colnames(vcp(example_pvol)))
  expect_type(vcp(example_pvol)$params, "list")
  expect_type(vcp(example_pvol)$params[[2]], "character")
})
test_that("select argument works", {
  expect_equal(setdiff(colnames(vcp(example_scan, add_params = F)), c(
    "how.lowprf",
    "how.midprf",
    "how.highprf",
    "where.elangle",
    "where.nbins",
    "where.nrays",
    "where.rscale",
    "how.NI"
  )), character(0))
  expect_equal(colnames(vcp(example_pvol, select = NULL, add_params = F)), names(unlist(example_pvol$scans[[1]]$attributes, recursive = F)))
})

test_that("silently isnores non existant colums", {
  expect_equal(setdiff(colnames(vcp(example_scan, select = c(
    "how.lowprf",
    "how.NI", "asdf"
  ), add_params = F)), c(
    "how.lowprf",
    "how.NI"
  )), character(0))
  expect_silent(vcp(example_scan, select = c(
    "how.lowprf",
    "how.NI", "asdf"
  )))
})
