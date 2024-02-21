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

preprocess_schema_constraints <- function(schema) {
  for (i in seq_along(schema$fields)) {
    if ("constraints" %in% names(schema$fields[[i]])) {
      constraints <- schema$fields[[i]]$constraints
      if ("minimum" %in% names(constraints) && constraints$minimum %in% c("Inf", "-Inf")) {
        schema$fields[[i]]$constraints$minimum <- ifelse(constraints$minimum == "Inf", Inf, ifelse(constraints$minimum == "-Inf", -Inf, constraints$minimum))
      }
      if ("maximum" %in% names(constraints) && constraints$maximum %in% c("Inf", "-Inf")) {
        schema$fields[[i]]$constraints$maximum <- ifelse(constraints$maximum == "Inf", Inf, ifelse(constraints$maximum == "-Inf", -Inf, constraints$maximum))
      }
    }
  }
  return(schema)
}

vpts_schema <- preprocess_schema_constraints(vpts_schema)


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
