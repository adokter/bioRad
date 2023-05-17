#' Title
#'
#' @param date_min
#' @param date_max
#' @param radars
#' @param format
#' @param source
#'
#' @return A character vector of aloft urls
#' @export
#'
#' @examples
list_vpts_aloft <- function(
    date_min = NULL,
    date_max = NULL,
    radars = NULL,
    format = "csv", # also hdf5
    source = "baltrad" # also ecog-04003
) {
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

  # check radars
  valid_radars <-
    jsonlite::fromJSON(
      "https://raw.githubusercontent.com/enram/aloftdata.eu/main/_data/OPERA_RADARS_DB.json"
    ) %>% dplyr::filter(!is.na(odimcode)) %>%
    dplyr::pull(odimcode)

  assertthat::assert_that(
    all(radars %in% valid_radars),
    msg = glue::glue("Can't find radar(s): {missing_radars}",
      missing_radars = radars[!radars %in% valid_radars]
    )
  )

  # create file list --------------------------------------------------------

  # Convert to dates
  start_date <- as.Date(date_min, tz = NULL)
  end_date <- as.Date(date_max, tz = NULL)

  # Set base URL
  base_url <- "https://aloft.s3-eu-west-1.amazonaws.com"

  if (format == "csv") {
    # Aloft CSV data are available in daily and monthly files
    # This function uses the zipped monthly files, which are faster to download
    months <- format(seq(start_date, end_date, by = "months"), "%Y%m")
    base_url <- "https://aloft.s3-eu-west-1.amazonaws.com"

    aws.s3::get_bucket_df(
      bucket = "s3://aloft",
      prefix = glue::glue("{source}/monthly"),
      region = "eu-west-1",
      max = Inf
    ) %>%
      dplyr::mutate(radar = purrr::map_chr(.data$Key, ~strsplit(.x, "/", fixed = TRUE)[[1]][3]),
                    date = regmatches(.data$Key, regexpr("[0-9]{6}", .data$Key))) %>%
      dplyr::filter(.data$radar %in% radars,
                    date %in% months) %>%
      dplyr::pull(Key) %>%
      paste(base_url, ., sep = "/")


  } else {
    # hdf5 files
    # TODO: create file paths of form
    # https://aloft.s3-eu-west-1.amazonaws.com/baltrad/hdf5/bejab/2023/05/02/bejab_vp_20230502T000000Z_0x9.h5
  }

}
