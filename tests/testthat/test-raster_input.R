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
							     xlim=c(-10010,10000),ylim=c(-11010,10000), res=210),'ppi')
		  expect_equal(a<-raster(integrate_to_ppi(example_pvol, example_vp, res=raster(my_ppi$data))$data),
			       b<-raster(my_ppi$data))
})
