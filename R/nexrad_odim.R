#' Convert a NEXRAD polar volume file to an ODIM polar volume file
#'
#' @param pvolfile_nexrad Character (vector). Either a path to a single radar polar volume
#'   (`pvol`) file containing multiple scans/sweeps, or multiple paths to scan
#'   files containing a single scan/sweep. Or a single `pvol` object. The file data format should be either 1)
#'   [ODIM](https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf)
#'    format, which is the implementation of the OPERA data information model in
#'   the [HDF5](https://support.hdfgroup.org/HDF5/) format, 2) a format
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
#' \dontrun{
#' # download a NEXRAD file, save as KBGM_example
#' download.file(paste("https://noaa-nexrad-level2.s3.amazonaws.com/",
#'   "2019/10/01/KBGM/KBGM20191001_000542_V06",
#'   sep = ""
#' ), "~/KBGM_example")
#'
#' # convert to ODIM format
#' nexrad_to_odim("~/KBGM_example", "~/KBGM_example.h5")
#'
#' # verify that we have generated a polar volume in ODIM HDF5 format
#' get_odim_object_type("~/KBGM_example.h5")
#'
#' # clean up
#' file.remove("~/KBGM_example", "~/KBGM_example.h5")
#' }
nexrad_to_odim <- function(pvolfile_nexrad, pvolfile_odim, verbose = FALSE) {
  assert_that(dir.exists(dirname(pvolfile_odim)),msg=paste("output directory", dirname(pvolfile_odim), "not found"))
  assert_that(is.writeable(dirname(pvolfile_odim)))

  config <- vol2birdR::vol2bird_config()
  vol2birdR::rsl2odim(file=pvolfile_nexrad, config=config, pvolfile_out=pvolfile_odim, verbose=verbose)
}
