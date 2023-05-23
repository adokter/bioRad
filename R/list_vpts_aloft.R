#' List aloft urls for time series of vertical profiles (`vpts`) of radar
#' stations
#'
#' @param date_min The first date to return urls for. In the shape of
#'   YYYY-MM-DD.
#' @param date_max The last date to return urls for. In the shape of YYYY-MM-DD.
#' @param radars A character vector of radar stations to return urls for.
#' @param format (Character) The format of archive urls to return, either csv or
#'   hdf5. Currently only csv urls are supported.
#' @param source (Character) Either `baltrad` or `ecog-04003`
#'
#' @return A character vector of aloft urls
#' @export
#'
#' @examples
#' list_vpts_aloft(radars = "bejab")
list_vpts_aloft <- function(
    date_min = NULL,
    date_max = NULL,
    radars = NULL,
    format = "csv", # also hdf5
    source = "baltrad" # also ecog-04003
) {
  # Check if aws.s3 is installed
  # NOTE added because aws.s3 is schedueled to be moved to Suggests

  rlang::check_installed("aws.s3",
    reason = "to connect to the aloft bucket on Amazon Web Services"
  )

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
    ) %>% dplyr::filter(!is.na(.data$odimcode)) %>%
    dplyr::pull("odimcode")

  assertthat::assert_that(
    all(radars %in% valid_radars),
    msg = glue::glue("Can't find radar(s): {missing_radars}",
      missing_radars = radars[!radars %in% valid_radars]
    )
  )

  # create file list --------------------------------------------------------

  # handle missing dates
  if(rlang::is_empty(date_min)){
    # if date_min is missing, set it to a date predating any radar observations
    date_min <- "1900-01-01"
  }
  if(rlang::is_empty(date_max)){
    date_max <- "2999-01-01"
  }

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

    found_vpts_aloft <-
      aws.s3::get_bucket_df(
        bucket = "s3://aloft",
        prefix = glue::glue("{source}/monthly"),
        region = "eu-west-1",
        max = Inf
      ) %>%
      dplyr::mutate(
        radar = purrr::map_chr(.data$Key, ~ strsplit(.x, "/", fixed = TRUE)[[1]][3]),
        date = regmatches(.data$Key, regexpr("[0-9]{6}", .data$Key))
      ) %>%
      dplyr::filter(
        .data$radar %in% radars,
        date %in% months
      )



  } else {
    # hdf5 files
    # TODO: create file paths of form
    # https://aloft.s3-eu-west-1.amazonaws.com/baltrad/hdf5/bejab/2023/05/02/bejab_vp_20230502T000000Z_0x9.h5
  }

  # Provide a warning if data coudn't be retreived for all requested radar
  # stations
  found_radars <-
    dplyr::distinct(found_vpts_aloft, .data$radar) %>%
    dplyr::pull("radar")
  all_radars_found <- all(found_radars == radars)
  if (!all_radars_found) {
    warning(
      assertthat::validate_that(
        all(found_radars == radars),
        msg = glue::glue(
          "Found no data for radars: {missing_radars_collapse}",
          missing_radars_collapse =
            glue::glue_collapse(
              glue::backtick(radars[!radars %in% found_radars]),
              sep = ", "
            )
        )
      )
    )
  }

  data_urls <-
    glue::glue("{base_url}/{keys}",
      keys = dplyr::pull(found_vpts_aloft, "Key")
    )

  # Assert that some data was found
  assertthat::assert_that(
    assertthat::not_empty(data_urls),
    msg = glue::glue("No data found for radars between {date_min} - {date_max}")
  )

  # Warn if less dates were found then requested
  if (!all(months %in% found_vpts_aloft$date)) {
    warning(
      glue::glue(
        "No radars found for all dates, ",
        "radars found for {first_date_found} to {last_date_found}",
        first_date_found = format(lubridate::ym(min(
          found_vpts_aloft$date
        )), "%Y-%m"),
        last_date_found = format(lubridate::ym(max(
          found_vpts_aloft$date
        )), "%Y-%m")
      )
    )
  }

  return(data_urls)
}