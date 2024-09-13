test_that("select on scans", {
  skip_if_no_tidyselect()
  data(example_scan)
  expect_equal(names(dplyr::select(example_scan, ZDR)$params), "ZDR")
  expect_equal(names(dplyr::select(example_scan, "ZDR")$params), "ZDR")
  expect_equal(names(dplyr::select(example_scan, starts_with("Z"))$params), "ZDR")
  expect_equal(names(dplyr::select(example_scan, starts_with("X"))$params), character())
})
test_that("select on pvols", {
  skip_if_no_tidyselect()
  pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
  example_pvol <- read_pvolfile(pvolfile)
  expect_equal(names(dplyr::select(example_pvol, ZDR)$scans[[2]]$params), "ZDR")
  expect_equal(names(dplyr::select(example_pvol, "ZDR")$scans[[2]]$params), "ZDR")
  expect_equal(names(dplyr::select(example_pvol, starts_with("Z"))$scans[[2]]$params), "ZDR")
  expect_equal(names(dplyr::select(example_pvol, starts_with("X"))$scans[[2]]$params), character())
  # Check it does the same to all scans
  expect_true(
    length(
      unique(
        lapply(
          dplyr::select(example_pvol, contains("H"))$scans,
          function(x) names(x$params)
        )
      )
    )
    == 1
  )
})
