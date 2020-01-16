context('test-raster_input.R')
test_that('scan to raster produces simular output',{
		  data(example_vpts)
		  expect_s4_class(b<-scan_to_raster(example_scan, ylim = c(55, 57), xlim = c(12, 13), res = .1),'RasterBrick')
		  expect_equal(b, scan_to_raster(example_scan,res=raster(b)))
})
test_that('integrate to ppi produces simular output',{
		  data(example_vp)
		  expect_true(file.exists( pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")))
		  expect_s3_class(example_pvol <- read_pvolfile(pvolfile),'pvol')
		  expect_s3_class(my_ppi <- integrate_to_ppi(example_pvol, example_vp, nx = 60, ny = 50),'ppi')
		  expect_equal(raster(integrate_to_ppi(example_pvol, example_vp, res=raster(my_ppi$data))$data),
			       raster(my_ppi$data))
})
test_that('integrate to ppi produces simular output when limits are set',{
		  data(example_vp)
		  expect_true(file.exists( pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")))
		  expect_s3_class(example_pvol <- read_pvolfile(pvolfile),'pvol')
		  expect_s3_class(my_ppi <- integrate_to_ppi(example_pvol,
							     example_vp,
							     xlim=c(-10010,10000),ylim=c(-11010,10000), res=410),'ppi')
		  expect_equal(raster(integrate_to_ppi(example_pvol, example_vp, res=raster(my_ppi$data))$data),
			       raster(my_ppi$data))
})
test_that('integrate to ppi produces same values on cropped raster',{
  data(example_vp)
  expect_true(file.exists( pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")))
  expect_s3_class(example_pvol <- read_pvolfile(pvolfile),'pvol')
  expect_s3_class(my_ppi <- integrate_to_ppi(example_pvol,
                                             example_vp,
                                             xlim=c(-10010,10000),ylim=c(-11010,10000), res=210),'ppi')
  expect_equal(values(raster(integrate_to_ppi(example_pvol, example_vp,
					      res=raster::crop(raster(my_ppi$data), extent(0,3000,1000,5000)))$data)),
               values(raster::crop(raster(my_ppi$data), extent(0,3000,1000,5000))))
})
test_that('check if other projection gives same result',{
  data(example_vp)
  expect_true(file.exists( pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")))
  expect_s3_class(example_pvol <- read_pvolfile(pvolfile),'pvol')
  expect_s3_class(my_ppi <- integrate_to_ppi(example_pvol,
                                             example_vp,
                                             xlim=c(-10010,10000),ylim=c(-11010,10000), res=510),'ppi')
  expect_s4_class(res<-raster::rasterFromXYZ(spTransform(as(my_ppi$data,'SpatialPointsDataFrame'),'+proj=longlat')[c(3,7),]),'RasterLayer')
  expect_silent(proj4string(res)<-'+proj=longlat')
  expect_equal(values(raster(integrate_to_ppi(example_pvol, example_vp, res=res)$data))[!is.na(values(res))],
               values(res)[!is.na(values(res))])
})
context('check raster input for project_as_ppi')
test_that('sample_polar works',{
  data("example_scan")
  a<-example_scan$params[[3]]

  expect_s4_class(b<-bioRad::sample_polar(example_scan$params[[3]],500,10000, xlim=12.9+c(-1,1), ylim=56.4+c(-1,1), project = F), 'SpatialGridDataFrame')
  expect_equivalent(b,sample_polar(example_scan$params[[3]],raster(b), project = F))
  expect_s4_class(bb<-bioRad::sample_polar(example_scan$params[[3]],5000,10000, xlim=12.9+c(-1,1), ylim=56.4+c(-1,1), project = T), 'SpatialGridDataFrame')
  expect_equivalent(bb,sample_polar(example_scan$params[[3]],raster(b), project = T))

})

test_that('sample_polar works',{
  data("example_scan")
  expect_s3_class(b<-project_as_ppi(example_scan,500,10000, project = F), 'ppi')
  expect_s3_class(bb<-project_as_ppi(example_scan,raster(b$data), project = F), 'ppi')
  expect_equal(bb$radar,b$radar)
  expect_equal(bb$datetime,b$datetime)
  expect_equivalent(bb$data,b$data)
  expect_s3_class(b<-project_as_ppi(example_scan,50,1000, project = T), 'ppi')
  expect_s3_class(bb<-project_as_ppi(example_scan,raster(b$data), project = T), 'ppi')
  expect_equal(bb$radar,b$radar)
  expect_equal(bb$datetime,b$datetime)
  expect_equivalent(bb$data,b$data)
})


test_that('sample_polar works from different projection',{
  data("example_scan")
  expect_s3_class(b<-project_as_ppi(example_scan,3000,5000, project = F), 'ppi')
  expect_s4_class(r<-rasterFromXYZ(spTransform(as(b$data,'SpatialPoints')[s<-c(1,11),],'+proj=longlat'),crs = '+proj=longlat'), 'RasterLayer')
  expect_s3_class(bb<-project_as_ppi(example_scan,r, project = F), 'ppi')
  expect_equivalent(b$data@data[s,], bb$data@data[c(1,4),])
})

