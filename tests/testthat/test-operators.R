test_that("param operators", {
  data(example_scan)
  example_param <- get_param(example_scan, "VRADH")
  expect_equal(example_param + 1, 1 + example_param)
  expect_equal(example_param + example_param, 2 * example_param)
  expect_equal(example_param + -1, example_param - 1)
  expect_equal(example_param / 2, example_param * .5)
  expect_equal(example_param * 1:480, 1:480 * example_param)
  expect_equal(matrix(example_param) * 1:480, matrix(1:480 * example_param))
})


test_that("scan operators", {
  data(example_scan)
  expect_equal(example_scan + 1, 1 + example_scan)
  expect_equal(example_scan * 1, example_scan)
  expect_equal(example_scan, example_scan * 1)
  expect_equal(2 * example_scan, example_scan * 2)
  expect_equal(example_scan + example_scan, 2 * example_scan)
  expect_equal(example_scan + -1, example_scan - 1)
  expect_equal(example_scan / 2, example_scan * .5)
  expect_equal(example_scan * 1:480, 1:480 * example_scan)
  r <- matrix(runif(prod(dim(example_scan)[-1])), nrow = dim(example_scan)[2])
  expect_equal(example_scan * r, r * example_scan)
  expect_equal(example_scan$params[["DBZH"]][4, 1:54] * r[4, 1:54],
               get_param(r * example_scan, "DBZH")[4, 1:54])
})


test_that("pvol operators", {
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  example_pvol <- read_pvolfile(pvolfile)
  expect_equal(example_pvol + 1, 1 + example_pvol)
  expect_equal(example_pvol * 1, example_pvol)
  expect_equal(example_pvol, example_pvol * 1)
  expect_equal(2 * example_pvol, example_pvol * 2)
  expect_equal(example_pvol + example_pvol, 2 * example_pvol)
  expect_equal(example_pvol + -1, example_pvol - 1)
  expect_equal(example_pvol / 2, example_pvol * .5)
  example_pvol$scans <- lapply(example_pvol$scans, function(x) {
    x$params <- x$params[3]
    return(x)
  })
  expect_equal(example_pvol * example_pvol, exp(log(example_pvol) + log(example_pvol)))
  expect_equal(example_pvol, exp(log(example_pvol)))
  expect_equal(example_pvol, sqrt((example_pvol^2)))
  expect_equal(log10(example_pvol), log(example_pvol, base = 10))
  expect_equal(Reduce("+", lapply(list(example_pvol, example_pvol), exp)),
               exp(example_pvol) + exp(example_pvol))
  expect_equal(get_scan(example_pvol * list(1.4, 1.7, 1.8), 1.5),
               get_scan(example_pvol, 1.5) * 1.7)
  expect_equal(get_scan(example_pvol * list(1.4 + 1:480, 1.7 + 1:480, 1.8 + 1:480), 1.5),
               get_scan(example_pvol, 1.5) * (1.7 + 1:480))
})
