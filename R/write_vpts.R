#' Write time series of vertical profiles (`vpts`) to a file in VPTS CSV format
#'
#' Exports `vpts` data to a CSV file following the standardized VPTS data exchange format. 
#' For more info, refer to the official documentation at https://aloftdata.eu/vpts-csv/.
#'
#' @param vpts An object of class `vpts` or an object of class `dataframe` containing the required fields of a VPTS CSV
#' @param file string. A filepath to write the CSV file will be saved, including the file name.
#' @param overwrite logical. Overwrites existing file when TRUE.
#' @return Invisible `NULL`. The function is called for its side effect of writing a file in VPTS CSV format.
#'
#' @examples
#' filepath <- "vpts_out.csv"
#' write_vpts(vpts_data = vpts_obj, file_path = vptsfile_path)
#'
#' @export
write_vpts <- function(vpts_data, file_path, include_header = TRUE) {




}
