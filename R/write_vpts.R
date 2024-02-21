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
write_vpts <- function(x, ...) {
  UseMethod("write_vpts")
}

#Get required field names
vpts_schema <- jsonlite::fromJSON(system.file("extdata", "vpts-csv-table-schema.json", package = "bioRad"),
    simplifyDataFrame = FALSE, simplifyVector = TRUE
)

required_fields <- lapply(vpts_schema$fields, function(field) {
  if (!is.null(field$constraints)) {
    if (!is.null(field$constraints$required) && field$constraints$required == TRUE) {
      return(field$name)
    }
  }
  return(NULL)
})

required_field_names <- required_fields[!sapply(required_fields, is.null)]
required_field_names <- unlist(required_field_names)


write_vpts.dataframe <- function(x, file, ...) {

preserve_nan <- function(x) {
  # Check if the value is NaN, and replace only NaN with "NaN" string
  ifelse(is.nan(x), "NaN", x)
}

 
}

write_vpts.vpts <- function(x, file, ...) {
  # Your code to handle writing a vpts object to a file
  message("Writing VPTS from vpts object to ", file)
  # Actual writing logic here
}
