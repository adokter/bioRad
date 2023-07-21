#' List aloft urls for time series of vertical profiles (`vpts`) of radar
#' stations
#'
#' @param date_min Character, the first date to return urls for. In the shape of
#'   YYYY-MM-DD.
#' @param date_max Character, the last date to return urls for. In the shape of
#'   YYYY-MM-DD.
#' @param radars Character vector, radar stations to return urls for.
#' @param format Character, the format of archive urls to return, either csv or
#'   hdf5. Currently only csv urls are supported.
#' @param source Character, either `baltrad` or `ecog-04003`
#' @param show_warnings Logical, whether to print warnings for dates or radar
#'   stations for which no data was found.
#'
#' @return A character vector of aloft urls
#' @export
#'
#' @examples
#' list_vpts_aloft(radars = "bejab")
list_vpts_aloft <- function(date_min = NULL,
                            date_max = NULL,
                            radars = NULL,
                            format = "csv", # also hdf5
                            source = "baltrad", # also ecog-04003
                            show_warnings = TRUE) {
  # Check if aws.s3 is installed
  # NOTE added because aws.s3 is schedueled to be moved to Suggests

  rlang::check_installed("aws.s3",
    reason = "to connect to the aloft bucket on Amazon Web Services"
  )

  # check arguments against vocabulary --------------------------------------
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
  aloft_radars_url <-
    paste(
      sep = "/",
      "https://raw.githubusercontent.com",
      "enram",
      "aloftdata.eu",
      "main",
      "_data",
      "OPERA_RADARS_DB.json"
    )
  valid_radars <- readr::read_lines(aloft_radars_url) %>%
    extract_string(pattern = '(?<="odimcode": ")[a-z]{5}', perl = TRUE)

  assertthat::assert_that(
    all(radars %in% valid_radars),
    msg = glue::glue("Can't find radar(s): {missing_radars}",
      missing_radars = radars[!radars %in% valid_radars]
    )
  )

  # create file list --------------------------------------------------------
  ## handle dates -----------------------------------------------------------

  # handle missing dates
  if (rlang::is_empty(date_min)) {
    # if date_min is missing, set it to a date predating any radar observations
    date_min <- "1900-01-01"
  }
  if (rlang::is_empty(date_max)) {
    date_max <- "9999-12-31"
  }

  # Convert to dates
  start_date <- as.Date(date_min, tz = NULL)
  end_date <- as.Date(date_max, tz = NULL)

  ## set static urls --------------------------------------------------------
  # Set base URL
  base_url <- "https://aloftdata.s3-eu-west-1.amazonaws.com"

  # format csv --------------------------------------------------------------
  if (format == "csv") {
    # Aloft CSV data are available in daily and monthly files
    # This function uses the zipped monthly files, which are faster to download
    months <- format(seq(start_date, end_date, by = "months"), "%Y%m")

    found_vpts_aloft <-
      aws.s3::get_bucket_df(
        bucket = "s3://aloftdata",
        prefix = glue::glue("{source}/monthly"),
        region = "eu-west-1",
        max = Inf
      ) %>%
      dplyr::mutate(
        radar = vapply(.data$Key, FUN = function(radar_key) {
          strsplit(radar_key, "/", fixed = TRUE)[[1]][3]
        }, FUN.VALUE = character(1)),
        date = extract_string(.data$Key, "[0-9]{6}")
      ) %>%
      dplyr::filter(
        .data$radar %in% radars,
        date %in% months
      )

  # format hdf5 -------------------------------------------------------------
  } else {
    # hdf5 files
    # TODO: create file paths of form
    # https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/hdf5/bejab/2023/05/02/bejab_vp_20230502T000000Z_0x9.h5
  }

  # format found data -------------------------------------------------------
  found_radars <-
    dplyr::distinct(found_vpts_aloft, .data$radar) %>%
    dplyr::pull("radar")

  data_urls <-
    glue::glue("{base_url}/{keys}",
      keys = dplyr::pull(found_vpts_aloft, "Key"),
      base_url = base_url
    )

  # warnings ----------------------------------------------------------------
  ## warn if no data found --------------------------------------------------
  if (rlang::is_empty(data_urls) && show_warnings) {
    warning(
      glue::glue("No data found for radars between {date_min} - {date_max}")
    )
    # stop here, no need to warn for radars and dates individually
    return(data_urls)
  }
  ## warn missing radar stations --------------------------------------------
  # Provide a warning if data couldn't be retrieved for all requested radar
  # stations

  all_radars_found <- all(found_radars == radars)
  if (!all_radars_found && show_warnings) {
    warning(
      glue::glue(
        "Found no data for radars: {missing_radars_collapse}",
        missing_radars_collapse =
          glue::glue_collapse(
            glue::backtick(radars[!radars %in% found_radars]),
            sep = ", "
          )
      )
    )
  }

  ## warn missing dates -----------------------------------------------------
  # Warn if less dates were found then requested
  if (!all(months %in% found_vpts_aloft$date) && show_warnings) {
    warning(
      glue::glue(
        "Not every date has radar data, ",
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

  # output vector of urls ---------------------------------------------------
  return(data_urls)
}
