test_that("param operators", {
  data(example_scan)
  example_param <- get_param(example_scan, "VRADH")
  expect_visible(example_param + 1)
  expect_equal(example_param + 1, 1 + example_param)
  expect_equal(example_param + example_param, 2 * example_param)
  expect_equal(example_param + -1, example_param - 1)
  expect_equal(example_param / 2, example_param * .5)
  expect_equal(example_param * 1:480, 1:480 * example_param)
  expect_equal(matrix(example_param) * 1:480, matrix(1:480 * example_param))
})

test_that("param and scan errors", {
  data("example_scan")
  p1<-get_param(example_scan,'VRADH')
  p2<-p1
  dim(p2)<-dim(p2)*c(2,.5)
  p3<-p2
  attr(p3,'datetime')<-attr(p3,'datetime')+10
  p4<-p3
  attr(p4,'geo')$height<-attr(p4,'geo')$height+10
  expect_error(p1 + p2, "Scan parameters have different dimensions")
  expect_silent(p2 + p3)
  expect_warning(p2 + p4, "*You are likely combining scan parameters with different elevations, radar locations or range/azimuth resolution")
  data("example_scan")
  s4 <- example_scan
  s2 <- example_scan
  s2$params <- list(DB = p2)
  s4$params <- list(DB = p4)
  expect_warning(s2 + s4, "*You are likely combining scan parameters with different elevations, radar locations or range/azimuth resolution")
  s5 <- example_scan
  s6 <- example_scan
  s5$params <- list(DDB = p2, DF = p2)
  s6$params <- list(DB = p2, DF = p2)
  expect_warning(s5 + s6, "potentially combining distinct parameters")
  s7 <- example_scan
  s7$params <- list(DB = p2, DF = p2, LL = p2)
  expect_error(s7 + s6, "not defined for scans with unequal number of scan parameters")
  expect_equal(s6 + s2, s6 + s6)
  expect_equal(s7 + s2, 2 * s7)
})

test_that("scan operators", {
  data(example_scan)
  expect_visible(example_scan + 1)
  expect_equal(example_scan + 1, 1 + example_scan)
  expect_equal(
    matrix(get_param(example_scan - 1, "VRADH")),
    matrix(get_param(example_scan, "VRADH")) - 1
  )
  expect_equal(
    matrix(get_param(1 - example_scan, "VRADH")),
    1 - matrix(get_param(example_scan, "VRADH"))
  )
  expect_equal(example_scan * 1, example_scan)
  expect_equal(example_scan, example_scan * 1)
  expect_equal(2 * example_scan, example_scan * 2)
  expect_equal(example_scan + example_scan, 2 * example_scan)
  expect_equal(example_scan + -1, example_scan - 1)
  expect_equal(example_scan / 2, example_scan * .5)
  expect_equal(example_scan * 1:480, 1:480 * example_scan)
  r <- matrix(runif(prod(dim(example_scan)[-1])), nrow = dim(example_scan)[2])
  expect_equal(example_scan * r, r * example_scan)
  expect_equal(
    example_scan$params[["DBZH"]][4, 1:54] * r[4, 1:54],
    get_param(r * example_scan, "DBZH")[4, 1:54]
  )
  expect_error(cumsum(example_scan), "not defined for scan objects")
})


test_that("pvol operators", {
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  example_pvol <- read_pvolfile(pvolfile)
  expect_silent(example_pvol + 1)
  expect_silent(1 + example_pvol)
  expect_silent(example_pvol + example_pvol)
  expect_visible(example_pvol + 1)
  expect_equal(exp(1)^example_pvol, exp(example_pvol))
  expect_equal(example_pvol + 1, 1 + example_pvol)
  expect_equal(
    matrix(get_param(get_scan(example_pvol - 1, 3), "DBZH")),
    matrix(get_param(get_scan(example_pvol, 3), "DBZH")) - 1
  )
  expect_equal(
    matrix(get_param(get_scan(1 - example_pvol, 3), "DBZH")),
    1 - matrix(get_param(get_scan(example_pvol, 3), "DBZH"))
  )
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
  expect_equal(
    Reduce("+", lapply(list(example_pvol, example_pvol), exp)),
    exp(example_pvol) + exp(example_pvol)
  )
  expect_equal(
    get_scan(example_pvol * list(1.4, 1.7, 1.8), 1.5),
    get_scan(example_pvol, 1.5) * 1.7
  )
  expect_equal(
    get_scan(example_pvol * list(1.4 + 1:480, 1.7 + 1:480, 1.8 + 1:480), 1.5),
    get_scan(example_pvol, 1.5) * (1.7 + 1:480)
  )
  expect_equal(
    get_scan(list(1.4 + 1:480, 1.7 + 1:480, 1.8 + 1:480) * example_pvol, 2.5),
    get_scan(example_pvol, 2.5) * (1.8 + 1:480)
  )
  expect_error(
    example_pvol * list(1, 2), "multiplication requires a list length equal to the number of scans."
  )
  expect_error(
    list(1, 2) * example_pvol, "multiplication requires a list length equal to the number of scans."
  )
  expect_error(cumsum(example_pvol), "not defined for pvol objects")
  p1 <- example_pvol
  p1$scans[2] <- NULL
  expect_error(p1 + example_pvol, "not defined for pvols with unequal number of scans")
  p2 <- example_pvol
  p2$scans <- p2$scans[c(1, 3, 2)]
  expect_warning(p2 + example_pvol, "*You are likely combining scan parameters with different elevations, radar locations or range/azimuth resolution")
})
test_that("Compare calculate_param and ops", {
  require(dplyr)
  data(example_scan)
  expect_equal(
    select(example_scan, "DBZH") - select(example_scan, "VRADH"),
    select(calculate_param(example_scan, DBZH = DBZH - VRADH), DBZH)
  )
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  example_pvol <- read_pvolfile(pvolfile, param = c("DBZH", "VRADH"))
  expect_equal(
    select(example_pvol, "DBZH") - select(example_pvol, "VRADH"),
    select(calculate_param(example_pvol, DBZH = DBZH - VRADH), DBZH)
  )
})
