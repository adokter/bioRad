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
test_that("vad raises cosine correction warnings and is applied",{
  expect_warning(plt_rg<-vad(pvol, cosine_correction="range_gates", range_min=5000, range_max=25000),'There are data that have a relatively low nyquist velocity')
  expect_warning(plt_vp<-vad(pvol,vp, cosine_correction="vp"),'The data to plot is based on multiple elevation angle')
  expect_equal(plt_rg$data$VRADH, plt_vp$data$VRADH *1/cospi(plt_vp$data$where.elangle/180))
  expect_identical(plt_vp$layers[[2]]$stat_params$args$v,vp$data$ff[2]* cospi(mean(get_elevation_angles(pvol))/180))
  expect_identical(plt_vp$layers[[3]]$stat_params$args$a,vp$data$dd[3])
  pvol_tmp<-pvol
  pvol_tmp$scans[[3]]$attributes$where$elangle<-20
  expect_warning(plt_none<-vad(pvol_tmp, vp),"Data with relatively large elevation angles are included.")
  expect_identical(plt_none$data$VRAD, plt_vp$data$VRADH)
  expect_s3_class(plt_scn<-vad(scan, vp),'ggplot')
  expect_equal(plt_scn$data,  plt_none$data |> dplyr::filter(scan_nr==1))
  expect_identical(plt_scn$layers[[2]]$stat_params$args$v,vp$data$ff[2]* cospi((get_elevation_angles(scan))/180))
})
