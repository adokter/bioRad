get_field_schema <- function(field, schema) {
    for (i in seq_along(schema$fields$name)) {
        if (field %in% c(schema$fields$name[i], schema$fields$nameAlternatives[[i]])) {
            return(schema$fields[i, ])
        }
    }
    return(NULL)  # return NULL if no matching field found
}
validate_datetime_format <- function(data, format) {
  parsed_data <- tryCatch({
    as.POSIXct(data, format = format, tz = "UTC")
  }, error = function(e) NULL)
  # check for failed parsing
  if (any(is.na(parsed_data))) {
    return(FALSE)
  }
  return(TRUE)
}
#' Validate dataframe against VPTS schema
#'
#' @param df The dataframe to validate.
#'
#' @return Invisibly returns a list with validation results including messages for any issues found.
#' @importFrom glue glue
#' @importFrom dplyr select
#' @examples
#' my_vpts <- as.data.frame(example_vpts)
#' validate_vpts(my_vpts)
#' @export
validate_vpts <- function(df) {
    schema <- vpts_schema
    required_fields <- schema$fields$name[schema$fields$constraints.required ==
        TRUE]
    all_fields <- schema$fields$name
    df_fields <- names(df)

    # Check for missing required fields
    missing_required <- setdiff(required_fields, df_fields)
    if (length(missing_required) > 0) {
        warning("Missing required fields: ", paste(missing_required, collapse = ", "))
    }

    issues <- list()
    extra_fields <- character()

    # Validate each field in the dataframe that is also in the schema
    for (field in df_fields) {

        field_schema <- get_field_schema(field, schema)
        if (!is.null(field_schema)) {

            field_data <- df[[field]]
            # Validate type
            type_valid <- switch(as.character(field_schema$type), string = is.character(field_data),
                number = is.numeric(field_data), integer = is.integer(field_data) ||
                  (is.numeric(field_data) && all(field_data == floor(field_data))),
                datetime = inherits(field_data, "POSIXct") || inherits(field_data,
                  "POSIXt"), boolean = is.logical(field_data), stop("Unsupported type specified in schema for field: ",
                  field))
            if (!type_valid) {
                issues <- c(issues, glue("Type validation failed for {field}"))
            }

            # Validate date-time format if specified
            if (field_schema$type == "datetime" && !is.na(field_schema$format)) {
              if (!validate_datetime_format(field_data, field_schema$format)) {
                return(glue("Date-time format validation failed for {field}"))
              }
            }

            # Validate constraints
            if (!is.null(field_schema$constraints)) {
                if (!is.na(field_schema$constraints$minimum) && any(field_data <
                  field_schema$constraints$minimum, na.rm = TRUE)) {
                  return(glue("Minimum value constraint violated for {field}"))
                }
                if (!is.na(field_schema$constraints$maximum) && any(field_data >
                  field_schema$constraints$maximum, na.rm = TRUE)) {
                  return(glue("Maximum value constraint violated for {field}"))
                c}
                if (!is.na(field_schema$constraints$pattern) && any(!stringr::str_detect(field_schema$constraints$pattern,
                  field_data))) {
                  return(glue("Pattern constraint violated for {field}"))
                }
            }
        } else {
            extra_fields <- c(extra_fields, field)
        }
    }

    # Show extra fields
    if (length(extra_fields) > 0) {
        warning("Extra fields found: ", paste(extra_fields, collapse = ", "))
    }

    # Show validation issues
    if (length(issues) > 0) {
        warning("Validation issues found: ", paste(results, collapse = "; "))
    }

    invisible(list(valid = TRUE, issues = issues))
}

