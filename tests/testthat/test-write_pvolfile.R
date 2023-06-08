pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

testpath <- file.path(tempdir(), basename(pvolfile))
file.copy(pvolfile, testpath) # Make copy of volume.h5 to avoid test leakage

test_that("write_pvolfile() returns error when writing no pvol object", {
  expect_error(write_pvolfile("No PVOL object"),
    regexp = "`pvol` must be an object of class `pvol`",
    fixed = TRUE
  )
})

test_that("write_pvolfile() returns error when file exists and overwrite = FALSE", {
  expect_error(
    write_pvolfile(pvol, testpath, overwrite = FALSE),
    regexp = "File already exists, use overwrite = TRUE to overwrite this file",
    fixed = TRUE
  )
})

test_that("write_pvolfile() writes a valid pvol", {
  write_pvolfile(pvol, testpath, overwrite = TRUE)
  expect_equal(get_odim_object_type(testpath), "PVOL")
})

test_that("write_pvolfile() writes data to the same dtype if infer_dtype = FALSE", {
  write_pvolfile(pvol, testpath, overwrite = TRUE, infer_dtype = FALSE)
  pvol_new <- read_pvolfile(testpath)

  extract_dtypes <- function(scan) {
    lapply(scan$params, function(x) attributes(x)$conversion$dtype)
  }

  old_dtypes <- lapply(pvol$scans, extract_dtypes)
  new_dtypes <- lapply(pvol_new$scans, extract_dtypes)
  expect_equal(old_dtypes, new_dtypes)
})

test_that("write_pvolfile() writes 8-bit integer if possible and conversion is missing", {
  pvol_noconversion <- pvol
  attributes(pvol_noconversion$scans[[1]]$params$DBZH)$conversion <- NULL
  write_pvolfile(pvol_noconversion, testpath, overwrite = TRUE)
  pvol_new <- read_pvolfile(testpath)
  expect_equal(
    attributes(pvol_new$scans[[1]]$params$DBZH)$conversion,
    attributes(pvol$scans[[1]]$params$DBZH)$conversion
  )
  expect_equal(
    attributes(pvol_new$scans[[1]]$params$DBZH)$conversion$dtype,
    "H5T_STD_U8BE"
  )
  expect_equal(
    matrix(pvol_noconversion$scans[[1]]$params[["DBZH"]]),
    matrix(pvol_new$scans[[1]]$params[["DBZH"]])
  )
})

test_that("write_pvolfile() writes 16-bit integer if possible and conversion is missing", {
  pvol_noconversion <- calculate_param(pvol, DBZH = DBZH * 100)
  attributes(pvol_noconversion$scans[[1]]$params$DBZH)$conversion <- NULL
  write_pvolfile(pvol_noconversion, testpath, overwrite = TRUE)
  pvol_new <- read_pvolfile(testpath)
  conv <- attributes(pvol_new$scans[[1]]$params$DBZH)$conversion
  expect_equal(conv$dtype, "H5T_STD_I16BE")
  expect_equal(conv$offset, -3001)
  expect_equal(conv$gain, 1)
  expect_equal(conv$nodata, 65535)
  expect_equal(conv$undetect, 0)
  expect_equal(
    matrix(pvol_noconversion$scans[[1]]$params[["DBZH"]]),
    matrix(pvol_new$scans[[1]]$params[["DBZH"]])
  )
})

test_that("write_pvolfile() writes float if values are not integers and conversion is missing", {
  pvol_noconversion <- calculate_param(pvol, DBZH = DBZH * 100.123)
  attributes(pvol_noconversion$scans[[1]]$params$DBZH)$conversion <- NULL
  write_pvolfile(pvol_noconversion, testpath, overwrite = TRUE)
  pvol_new <- read_pvolfile(testpath)
  conv <- attributes(pvol_new$scans[[1]]$params$DBZH)$conversion
  expect_equal(conv$dtype, "H5T_IEEE_F32BE")
  expect_equal(conv$offset, -3005)
  expect_equal(conv$gain, 1)
  expect_equal(conv$nodata, 8513)
  expect_equal(conv$undetect, 0)
  expect_equal(matrix(pvol_noconversion$scans[[1]]$params[["DBZH"]]),
    matrix(pvol_new$scans[[1]]$params[["DBZH"]]),
    tolerance = 1e-5
  )
})
