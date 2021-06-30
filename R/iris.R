#' Check the `task` type of an IRIS RAW file
#'
#' Checks what type of `task`(s), i.e. polar volume types, are contained in an
#' IRIS RAW file.
#'
#' @param file Character. Path to a polar volume file in IRIS RAW format.
#' @param header_size Integer. Number of header bytes to search.
#' @param task Character (vector). Task names to search for in the file header.
#'
#' @return Specified `task` names found in the header or `NA` if none of the
#'   task names were found.
#'
#' @export
get_iris_raw_task <- function(file, header_size = 50,
                              task = c("WIND", "SURVEILLANCE", "VOL_A", "VOL_B")) {
  assert_that(file.exists(file))

  # read binary header
  types <- sapply(task, function(x) suppressWarnings(
    any(grepl(pattern = x, readBin(file, n = header_size, "character")))))

  if(!any(types)) return(NA)

  if(sum(types) > 1) {
    warning(paste("Multiple file types found in binary header:",
                  paste(task[types], collapse = ",")))
  }

  task[types]
}
