context("test-integrate_to_ppi.R")

test_that("Still works with HGHT vps", {
  # this needs to work for backward compatibility with calculated vp file eg. enram
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  example_pvol <- read_pvolfile(pvolfile)
  data(example_vp)
  example_vpHGHT<-example_vp
  example_vpHGHT$data$HGHT<-example_vpHGHT$data$height
  example_vpHGHT$data$height<-NULL
  expect_equal(integrate_to_ppi(example_pvol, example_vp, nx = 20, ny=20),
               integrate_to_ppi(example_pvol, example_vpHGHT, nx = 20, ny=20))
})
