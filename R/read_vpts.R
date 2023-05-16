#' Read VPTS data from file(s)
#'
#' @param files Path(s) to one or more files containing vpts data.
#'   The following file formats are supported:
#'   - [VPTS CSV](https://aloftdata.eu/vpts-csv/).
#'   - [ODIM bird profile format](https://github.com/adokter/vol2bird/wiki/ODIM-bird-profile-format-specification).
#'   Formats cannot be mixed
#' @return VPTS data frame.
read_vpts <- function(files) {
  # Create Frictionless Data package
  package <- create_vpts_package(files)

  # Read vpts resource: binds rows of all CSV files
  data <- frictionless::read_resource(package, "vpts")

  # Check data and warn about issues
  # check_vpts()

  data
}

#' Create a Frictionless Data Package from VPTS CSV files
#'
#' @inheritParams read_vpts
#' @return List describing a Data Package.
#' @noRd
create_vpts_package <- function(files) {
  package <- frictionless::create_package()
  schema_v1 <- "https://raw.githubusercontent.com/enram/vpts-csv/main/vpts-csv-table-schema.json"
  package <- frictionless::add_resource(
    package,
    "vpts",
    data = files,
    schema = schema_v1
  )
  package
}
