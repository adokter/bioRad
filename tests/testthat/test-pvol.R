pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)
vp <- example_vp

# No tests for error on incorrect parameters:
# summary(), print() are generic and work for every input
# is.pvol() returns TRUE/FALSE and works for every input

# print.pvol() is not tested as it is the same as and called from summary.pvol()

test_that("summary.pvol() prints metadata to the console", {
  expect_output(summary(pvol), "Polar volume (class pvol)", fixed = TRUE)
  expect_output(summary(pvol), "# scans:", fixed = TRUE)
  expect_output(summary(pvol), "radar:", fixed = TRUE)
  expect_output(summary(pvol), "source:", fixed = TRUE)
  expect_output(summary(pvol), "nominal time:", fixed = TRUE)

})

test_that("is.pvol() returns TRUE/FALSE correctly", {
  expect_true(is.pvol(pvol))
  expect_false(is.pvol("not_a_pvol"))
  expect_false(is.pvol(vp))
})
