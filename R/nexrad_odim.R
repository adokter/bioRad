#' Convert a NEXRAD polar volume file to an ODIM polar volume file
#'
#' @param pvolfile_nexrad Character (vector). Either a path to a single radar polar volume
#'   (`pvol`) file containing multiple scans/sweeps, or multiple paths to scan
#'   files containing a single scan/sweep. Or a single `pvol` object. The file data format should be either 1)
#'   [ODIM](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf)
#'    format, which is the implementation of the OPERA data information model in
#'   the [HDF5](https://www.hdfgroup.org/solutions/hdf5/) format, 2) a format
#'   supported by the [RSL
#'   library](https://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/) or 3) Vaisala
#'   IRIS (IRIS RAW) format.
#' @param pvolfile_odim Filename for the polar volume in ODIM HDF5 format to be
#' generated.
#'
#' @inheritParams calculate_vp
#'
#' @return `TRUE` on success
#'
#' @export
#' @examples
#' \donttest{
#' # download a NEXRAD file, save as KBGM_example
#' path = file.path(tempdir(), "KBGM_example")
#' 
#' download.file(paste0("https://noaa-nexrad-level2.s3.amazonaws.com/",
#'   "2019/10/01/KBGM/KBGM20191001_000542_V06"), path, method="libcurl", mode="wb")
#'
#' # convert to ODIM format
#' 
#' new_path = file.path(tempdir(), "KBGM_example.h5")
#'
#' if (requireNamespace("vol2birdR", quietly = TRUE)) {
#' nexrad_to_odim(path, new_path)
#'
#' # verify that we have generated a polar volume in ODIM HDF5 format
#' get_odim_object_type(new_path)
#'
#' # clean up
#' file.remove(new_path)
#' }
#' file.remove(path)
#' }
nexrad_to_odim <- function(pvolfile_nexrad, pvolfile_odim, verbose = FALSE) {
  assertthat::assert_that(dir.exists(dirname(pvolfile_odim)),msg=paste("output directory", dirname(pvolfile_odim), "not found"))
  assertthat::assert_that(assertthat::is.writeable(dirname(pvolfile_odim)))

  config <- vol2birdR::vol2bird_config()
  vol2birdR::rsl2odim(file=pvolfile_nexrad, config=config, pvolfile_out=pvolfile_odim, verbose=verbose)
}
