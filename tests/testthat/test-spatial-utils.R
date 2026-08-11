test_that("coordinate transformations preserve sp results", {
  lon <- c(12.5, 13.3)
  lat <- c(56.0, 56.8)
  crss <- c(
    "+proj=aeqd +lat_0=56.4 +lon_0=12.9 +units=m",
    "EPSG:3857"
  )

  for (crs in crss) {
    projected <- bioRad:::wgs_to_proj(lon, lat, crs)
    legacy <- sp::SpatialPoints(
      unname(cbind(lon, lat)),
      proj4string = sp::CRS("+proj=longlat +datum=WGS84")
    )
    legacy <- sp::spTransform(legacy, sp::CRS(crs))

    expect_s3_class(projected, "sf")
    expect_equal(sf::st_crs(projected), sf::st_crs(crs))
    expect_equal(
      unname(sf::st_coordinates(projected)),
      unname(sp::coordinates(legacy)),
      tolerance = 1e-6
    )

    geographic <- bioRad:::proj_to_wgs(
      sf::st_coordinates(projected)[, "X"],
      sf::st_coordinates(projected)[, "Y"],
      crs
    )
    expect_s3_class(geographic, "sf")
    expect_equal(
      unname(sf::st_coordinates(geographic)),
      unname(cbind(lon, lat)),
      tolerance = 1e-7
    )
    expect_equal(sf::st_crs(geographic), sf::st_crs(4326))
  }
})

test_that("proj_to_wgs() reports transformation errors consistently", {
  expect_error(
    bioRad:::proj_to_wgs(0, 0, "invalid crs"),
    "proj_to_wgs\\(\\) failed"
  )
})
