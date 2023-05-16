#' Download vertical profile time series data (`vpts`) from aloftdata.eu
#'
#' @param directory description
#' @return
#' @family
#' @export
#' @examples
#' # example code
download_vpts_aloft <- function(directory = ".", radars = NULL,
                                start_date = NULL, end_date = NULL,
                                source = "baltrad", format = "csv",
                                overwrite = FALSE) {
  # Check source
  valid_sources <- c("baltrad", "ecog-04003")
  assertthat::assert_that(
    source %in% valid_sources,
    msg = glue::glue(
      "`source` must be one of: {valid_sources_collapse}.",
      valid_sources_collapse = glue::glue_collapse(
        glue::backtick(valid_sources), sep = ", "
      )
    )
  )

  # Check format
  valid_formats <- c("csv", "hdf5")
  assertthat::assert_that(
    format %in% valid_formats,
    msg = glue::glue(
      "`format` must be one of: {valid_formats_collapse}.",
      valid_formats_collapse = glue::glue_collapse(
        glue::backtick(valid_formats), sep = ", "
      )
    )
  )

  # Convert to dates
  start_date <- as.Date(start_date, tz = NULL)
  end_date <- as.Date(end_date, tz = NULL)

  # Set base URL
  base_url <- "https://aloft.s3-eu-west-1.amazonaws.com"

  # Create file list
  if (format == "csv") {
    # Aloft CSV data are available in daily and monthly files
    # This function uses the zipped monthly files, which are faster to download
    months <- format(seq(start_date, end_date, by = "months"), "%Y%m")

    # TODO: create file path of form
    # https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202302.csv.gz

    # This alternates incorrectly, creating too few file paths
    paste0("radar/", substring(month_range, 1, 4), "/", "bejab_vpts_", month_range, ".csv.gz")

    # Alternatively, use aws.s3 bucket
    # Get list of files
    # get_bucket_df(bucket = "s3://aloft", prefix="baltrad/monthly/bejab", region = "eu-west-1", max = 2000)
    # Save a file
    # save_object("baltrad/monthly/bejab/2023/bejab_vpts_202302.csv.gz", bucket = "s3://aloft/", region = "eu-west-1", file = "aloft/bejab.csv.gz")

    } else {
    # hdf5 files
    # TODO: create file paths of form
    # https://aloft.s3-eu-west-1.amazonaws.com/baltrad/hdf5/bejab/2023/05/02/bejab_vp_20230502T000000Z_0x9.h5
  }

  # Create directory if it doesn't exists yet
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
}
