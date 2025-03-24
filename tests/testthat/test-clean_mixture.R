# create vp and vpts object and add dummy wind values
vp <- example_vp
vp$data$u_wind <-seq(-1,-8, length.out=dim(vp)[1])
vp$data$v_wind <-seq(0,-3, length.out=dim(vp)[1])

vpts <- example_vpts
vpts$data$u_wind <- matrix(data=seq(0,10, length.out=dim(vpts)[2]),nrow=dim(vpts)[2],ncol=dim(vpts)[1])
vpts$data$v_wind <- matrix(data=seq(-2,6, length.out=dim(vpts)[2]),nrow=dim(vpts)[2],ncol=dim(vpts)[1])

test_that("clean_mixture() requires correct input data and parameters", {
  expect_error(clean_mixture(example_vp), "requires paired wind data")
  expect_error(clean_mixture(example_vpts), "requires paired wind data")
  expect_error(clean_mixture(vpts, slow=10, fast=1), "`slow` should be smaller than `fast`")
})

test_that("clean_mixture() returns vp or vpts object", {
  expect_s3_class(clean_mixture(vpts), "vpts")
  expect_s3_class(clean_mixture(vp), "vp")
})

test_that("clean_mixture() accepts temporally varying airspeed thresholds", {
  fast_ts=6+2*runif(dim(vpts)[1])
  slow_ts=runif(dim(vpts)[1])
  expect_s3_class(clean_mixture(vpts, fast=fast_ts), "vpts")
  expect_s3_class(clean_mixture(vpts, slow=slow_ts), "vpts")
  expect_s3_class(clean_mixture(vpts, slow=slow_ts, fast=fast_ts), "vpts")
})

test_that("clean_mixture() can read slow and fast airspeeds from profile data columns", {
  slow_test=1
  fast_test=8
  vpts_df <- as.data.frame(vpts, suntime=FALSE)
  vpts_df$slow = slow_test
  vpts_df$fast = fast_test
  expect_warning(vpts_test <- as.vpts(vpts_df), "Extra fields found")
  expect_s3_class(clean_mixture(vpts_test, slow="slow"), "vpts")
  expect_s3_class(clean_mixture(vpts_test, fast="fast"), "vpts")
  expect_s3_class(clean_mixture(vpts_test, slow="slow", fast="fast"), "vpts")
  expect_equal(clean_mixture(vpts_test, slow="slow", fast="fast")$data$dens, clean_mixture(vpts, slow=slow_test, fast=fast_test)$data$dens)
})

test_that("clean_mixture() returns warning for infinite wind data", {
  vp$data$u_wind[3] <- NA
  expect_warning(clean_mixture(vp), "wind contains non finite values")
})

test_that("clean_mixture() returns expected values", {
  # only birds
  expect_equal(clean_mixture(100,u=10,v=0,U=5,V=0, fast=5, slow=1)$f,0)
  # only insects
  expect_equal(clean_mixture(100,u=6,v=0,U=5,V=0, fast=5, slow=1)$f,1)
  # observed airspeed faster than fast component
  expect_equal(clean_mixture(100,u=11,v=0,U=5,V=0, fast=5, slow=1)$f,0)
  # observed airspeed slower than slow component
  expect_equal(clean_mixture(100,u=5.5,v=0,U=5,V=0, fast=5, slow=1)$f,1)
})


