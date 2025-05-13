#' Download vertical profile (`vp`) files from the ENRAM data repository
#'
#' Download and unzip a selection of vertical profile (`vp`) files from the
#' [ENRAM data repository](https://aloftdata.eu/), where
#' these are stored as monthly zips per radar.
#'
#' @note `download_vpfiles()` will be deprecated in a future version. See `getRad::get_vpts()` instead.
#'
#' @param date_min Character. Start date of file selection, in `YYYY-MM-DD`
#'   format. Days will be ignored.
#' @param date_max Character. End date of file selection, in `YYYY-MM-DD`
#'   format. Days will be ignored.
#' @param radars Character (vector). 5-letter country/radar code(s) to include
#'   in file selection.
#' @param directory Character. Path to local directory where files should be
#'   downloaded and unzipped.
#' @param overwrite Logical. When `TRUE`, re-download and overwrite previously
#'   downloaded files of the same names.
#' @return `NULL`. The function's primary effect is to download selected vertical profiles
#' files from ENRAM data repository to a specified local directory, and to provide
#' a message and a progress bar in the console indicating the download status. Message will show
#' a 404 error for files that are not available.
#' @export
#'
#' @seealso
#' * [read_vpts()]
#' * [select_vpfiles()]
#' * [read_vpfiles()]
#'
#' @examples
#' \donttest{
#' # Download (and overwrite) data from radars "bejab" and "bewid".
#' download_vpfiles(
#'   date_min = "2018-10-01",
#'   date_max = "2018-10-31",
#'   radars = c("bejab", "bewid"),
#'   directory = tempdir(),
#'   overwrite = TRUE
#' )
#' }
download_vpfiles <- function(date_min, date_max, radars, directory = ".",
                             overwrite = FALSE) {

lifecycle::deprecate_warn("2025", "download_vpfiles()", "getRad::get_vpts()")

  # Ensure directory exists
  assertthat::assert_that(assertthat::is.dir(directory))

  # Stop if radar codes are not exactly 5 characters
  check_radar_codes(radars)

  # Stop if dates are not a string
  assertthat::assert_that(assertthat::is.string(date_min))
  assertthat::assert_that(assertthat::is.string(date_max))

  # Stop if dates are not in YYYY-MM-DD format:
  check_date_format(date_min, "%Y-%m-%d")
  check_date_format(date_max, "%Y-%m-%d")

  # Stop if overwrite is not a logical
  assertthat::assert_that(is.logical(overwrite), msg='overwrite is not a logical')

  # Set day to 01 and create series of yyyy/mm based on date_min/max:
  # 2016/10, 2016/11, 2016/12
  dates <- seq(
    as.Date(paste(substring(date_min, 1, 7), "01", sep = "-"), tz = NULL),
    as.Date(paste(substring(date_max, 1, 7), "01", sep = "-"), tz = NULL),
    by = "months"
  )
  year_months <- format(dates, "%Y/%m")

  # Expand to series of radar/yyyy/mm: be_jab/2016/10, be_wid/2016/10, ...
  radar_year_months <- apply(expand.grid(radars, year_months), 1, paste,
    collapse = "/"
  )

  # Set base url of data repository
  base_url <- "https://aloftdata.s3-eu-west-1.amazonaws.com"

  # Start download and unzipping
  message(paste("Downloading data from", base_url))

  for (radar_year_month in radar_year_months) {
    # Create filename of format bejab201610.zip (removing _ and /)
    file_name <- paste0(gsub("/", "", radar_year_month), ".csv.gz")
    # insert new _vpts_ label (FIXME: assumes 5 letter radar code)
    file_name <- paste0(substr(file_name,1,5),"_vpts_",substr(file_name,6,nchar(file_name)))
    # Create filepath of format directory/bejab_vpts_201610.csv.gz
    file_path <- file.path(directory, file_name)
    # Create url of format base_url/be/jab/2016/bejab_vpts_201610.csv.gz (removing month)
    url <- paste(
      base_url,"baltrad/monthly",
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
    req <- curl::curl_fetch_disk(url, file_path) # will download regardless of status

    # Check http status
    if (req$status_code == "200") {
      # Unzip file no longer necessary
      # utils::unzip(file_path, exdir = file.path(directory, unzip_dir))
      message(paste0(file_name, ": successfully downloaded"))
    } else {
      # Remove file
      unlink(file_path)
      message(paste0(file_name, ": http error ", req$status_code))
    }
  }
}
