pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)
vp <- example_vp
scan<-example_scan

test_that("vad() errors on incorrect parameters", {
  expect_error(vad(vp), "no applicable method for 'vad' applied to an object of class \"vp\"")
  expect_error(vad(pvol, 1), "is.vp(x = vp) is not TRUE", fixed = T)
  expect_error(
    vad(pvol, range_max = "a"),
    "range_max is not a numeric or integer vector"
  )
  expect_error(vad(pvol, range_max = 1:2),
    "range_max is not NULL or rlang::is_scalar_vector(x = range_max) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    vad(pvol, range_min = "a"),
    "range_min is not a numeric or integer vector"
  )
  expect_error(vad(pvol, range_min = 1:2),
    "range_min is not NULL or rlang::is_scalar_vector(x = range_min) is not TRUE",
    fixed = TRUE
  )

  expect_error(
    vad(pvol, alt_max = "a"), "alt_max is not a numeric or integer vector"
  )
  expect_error(
    vad(pvol, alt_max = 1:2), "alt_max is not NULL or rlang::is_scalar_vector(x = alt_max) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    vad(pvol, alt_min = "a"), "alt_min is not a numeric or integer vector"
  )
  expect_error(
    vad(pvol, alt_min = 1:2), "alt_min is not NULL or rlang::is_scalar_vector(x = alt_min) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    vad(pvol, vp, range_min = 1),
    "When specifying a vp the range is taken from there. Thus no range should be provided"
  )
  expect_error(
    vad(pvol, vp, range_max = 1),
    "When specifying a vp the range is taken from there. Thus no range should be provided"
  )
})
test_that("plot from vad() matches some expectations", {
  expect_s3_class(plt <- vad(pvol,
    alt_min = 400, alt_max = 756,
    range_min = 6953, range_max = 65344,
    range_gate_filter= where.elangle<2
  ), "ggplot")
  expect_s3_class(plt$facet, "FacetNull")
  expect_true(all(plt$data$height < 756))
  expect_true(all(plt$data$height > 400))
  expect_true(all(plt$data$range < 65344))
  expect_true(all(plt$data$range > 6953))
  expect_equal(plt$mapping, ggplot2::aes(x = azim, y = VRADH), ignore_attr = TRUE)
  expect_length(plt$data$scan_nr|> unique(),sum(get_elevation_angles(pvol)<2))

})

test_that("plot from vad() matches some expectations", {
  expect_s3_class(plt <- vad(calculate_param(pvol, VRAD = VRADH), vp), "ggplot")
  expect_s3_class(plt$facet, "FacetWrap")
  expect_equal(plt$mapping, ggplot2::aes(x = azim, y = VRAD), ignore_attr = TRUE)
  expect_length(plt$data$scan_nr|> unique(),length(pvol$scans))

})

test_that("plot from vad() matches some expectations for scans", {
  expect_s3_class(plt <- vad(scan, vp), "ggplot")
  expect_equal(plt$mapping, ggplot2::aes(x = azim, y = VRADH), ignore_attr = TRUE)
  expect_length(plt$data$scan_nr|> unique(),1)
})
test_that("vad raises cosine correction warnings",{
expect_warning(vad(pvol, cosine_correction="range_gates"),'There are data that have a relatively low nyquist velocity')
  expect_warning(vad(pvol,vp, cosine_correction="vp"),'The data to plot is based on multiple elevation angle')
  pvol_tmp<-pvol
  pvol_tmp$scans[[3]]$attributes$where$elangle<-20
  expect_warning(vad(pvol_tmp),"Data with relativiely large elevation angles are included.")
})
