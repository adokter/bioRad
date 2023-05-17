#' Check task of IRIS RAW file
#'
#' Checks which task (polar volume type) is contained in a IRIS RAW file
#'
#' @param file A string containing a file name.
#' @param header_size Number of header bytes to search
#' @param task task names to search for in the file header
#' @return one of the `task` names found in the header, `NA` if none of the task names were found.
#'
#' @export
get_iris_raw_task <- function(file, header_size=50, task=c("WIND","SURVEILLANCE","VOL_A","VOL_B")){
  assert_that(file.exists(file))

  # read binary header
  types <- sapply(task, function(x) suppressWarnings(any(grepl(pattern=x,readBin(file,n=header_size,"character")))))

  if(!any(types)) return(NA)

  if(sum(types) > 1) warning(paste("multiple file types found in binary header:",paste(task[types],collapse = ",")))

  task[types]
}
