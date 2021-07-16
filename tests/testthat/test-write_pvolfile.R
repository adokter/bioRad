pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
pvol <- read_pvolfile(pvolfile)

testpath <- file.path(tempdir(), basename(pvolfile))
file.copy(pvolfile, testpath)  # Make copy of volume.h5 to avoid test leakage

test_that("write_pvolfile() returns error when writing no pvol object", {
  expect_error(write_pvolfile("No PVOL object"))
})

test_that("write_pvolfile() returns error when file exists and overwrite = FALSE", {
  expect_error(write_pvolfile(pvol, testpath, overwrite = FALSE))
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


