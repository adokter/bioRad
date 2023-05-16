#' Download vertical profile (`vp`) files from the ENRAM data repository
#'
#' Download and unzip a selection of vertical profile (`vp`) files from the
#' [ENRAM data repository](https://aloftdata.eu/), where
#' these are stored as monthly zips per radar.
#'
#' @param date_min character. YYYY-MM-DD start date of file selection. Days will
#'   be ignored.
#' @param date_max character. YYYY-MM-DD end date of file selection. Days will
#'   be ignored.
#' @param radars character (vector). 5-letter country/radar code(s)
#'   (e.g. "bejab") of radars to include in file selection.
#' @param directory character. Path to local directory where files should be
#'   downloaded and unzipped.
#' @param overwrite logical. TRUE for re-downloading and overwriting previously
#'   downloaded files of the same names.
#'
#' @export
#' @importFrom curl curl_fetch_disk
#'
#' @seealso select_vpfiles
#'
#' @examples
#' # Download data from radars "bejab" and "bewid", even if previously
#' # downloaded (overwrite = TRUE). Will successfully download 2016-10 files,
#' # but show 404 error for 2016-11 files (as these are not available).
#' \dontrun{
#' dir.create("~/bioRad_tmp_files")
#' download_vpfiles(
#'   date_min = "2016-10-01",
#'   date_max = "2016-11-30",
#'   radars = c("bejab", "bewid"),
#'   directory = "~/bioRad_tmp_files",
#'   overwrite = TRUE
#' )
#' # clean up:
#' unlink("~/bioRad_tmp_files", recursive = T)
#' }
download_vpfiles <- function(date_min, date_max, radars, directory = ".",
                             overwrite = FALSE) {
  # Ensure directory exists
  assert_that(is.dir(directory))

  # Stop if radar codes are not exactly 5 characters
  check_radar_codes(radars)

  # Split 5 letter radar codes into format be_jab
  ra_dars <- paste(substring(radars, 1, 2), substring(radars, 3, 5), sep = "_")

  # Stop if dates are not a string
  assert_that(is.string(date_min))
  assert_that(is.string(date_max))

  # Stop if dates are not in YYYY-MM-DD format:
  check_date_format(date_min, "%Y-%m-%d")
  check_date_format(date_max, "%Y-%m-%d")

  # Stop if overwrite is not a logical
  assert_that(is.logical(overwrite), msg='overwrite is not a logical')

  # Set day to 01 and create series of yyyy/mm based on date_min/max:
  # 2016/10, 2016/11, 2016/12
  dates <- seq(
    as.Date(paste(substring(date_min, 1, 7), "01", sep = "-"), tz = NULL),
    as.Date(paste(substring(date_max, 1, 7), "01", sep = "-"), tz = NULL),
    by = "months"
  )
  year_months <- format(dates, "%Y/%m")

  # Expand to series of radar/yyyy/mm: be_jab/2016/10, be_wid/2016/10, ...
  radar_year_months <- apply(expand.grid(ra_dars, year_months), 1, paste,
    collapse = "/"
  )

  # Set base url of data repository
  base_url <- "https://lw-enram.s3-eu-west-1.amazonaws.com"

  # Start download and unzipping
  message(paste("Downloading data from", base_url))

  for (radar_year_month in radar_year_months) {
    # Create filename of format bejab201610.zip (removing _ and /)
    file_name <- paste0(gsub("/", "", gsub("_", "", radar_year_month)), ".zip")
    # Create filepath of format directory/bejab201610.zip
    file_path <- file.path(directory, file_name)
    # Create url of format base_url/be/jab/2016/bejab201610.zip (removing month)
    url <- paste(
      base_url,
      gsub(
        "_", "/",
        substring(radar_year_month, 1, nchar(radar_year_month) - 3)
      ),
      file_name,
      sep = "/"
    )
    # Create local unzip directory of format directory/bejab/2016/10
    unzip_dir <- gsub("_", "", radar_year_month)

    # Skip download if overwrite = FALSE and file already exists locally
    if (file.exists(file_path) && overwrite == FALSE) {
      message(paste0(file_name, ": already downloaded"))
      next
    }

    # Start download
    req <- curl_fetch_disk(url, file_path) # will download regardless of status

    # Check http status
    if (req$status_code == "200") {
      # Unzip file
      unzip(file_path, exdir = file.path(directory, unzip_dir))
      message(paste0(file_name, ": successfully downloaded"))
    } else {
      # Remove file
      unlink(file_path)
      message(paste0(file_name, ": http error ", req$status_code))
    }
  }
}

#' Check if radar codes are exactly 5 characters
#'
#' @param radars character vector. Radar codes to check, e.g. `c("bejab",
#'   "bewideu")`.
#'
#' @return NULL. Will stop and show error message if at least one of the
#'   provided radar codes is not exactly 5 characters.
#'
#' @keywords internal
check_radar_codes <- function(radars) {
  wrong_codes <- radars[nchar(radars) != 5]
  if (length(wrong_codes) > 0) {
    stop(
      "Radar codes should be 5 characters: ",
      paste(wrong_codes, collapse = ", ")
    )
  } else {
    radars.csv <- read.csv(url("https://lw-enram.s3-eu-west-1.amazonaws.com/radars.csv"))
    wrong_codes <- radars[!(radars %in% radars.csv$countryradar)]
    if (length(wrong_codes) > 0) {
      stop(
        "Radar codes don't exist: ",
        paste(wrong_codes, collapse = ", ")
      )
    }
  }
}

#' Check if character date is in specific format
#'
#' @param date character. Character representation of a date, e.g.
#'   `"2018-12-13"`.
#' @param format character. strptime format the date should have, e.g.
#'   `"\%Y-\%m-\%d"`
#'
#' @return NULL. Will stop and show error message if date does not have correct
#'   date format.
#'
#' @keywords internal
check_date_format <- function(date, format) {
  parsed_date <- as.Date(date, format = format, tz = NULL)
  if (is.na(parsed_date)) {
    stop("Incorrect date format: ", date)
  }
}
