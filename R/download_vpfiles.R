#' Download vertical profile files (\code{vp}) from the ENRAM data repository
#'
#' Download and unzip a selection of vertical profile files (\code{vp}) from the
#' \href{http://enram.github.io/data-repository/}{ENRAM data repository}, where
#' these are stored as monthly zips per radar.
#'
#' @param date_min character. ISO 8601 start date of data selection. Days will
#'   be ignored.
#' @param date_max character. ISO 8601 end date of data selection. Days will be
#'   ignored.
#' @param radar character (vector). 5-letter country/radar code(s)
#'   (e.g. "bejab") of radars to include in data selection.
#' @param directory character. Path to local directory for downloading and
#'   unzipping files.
#' @param overwrite logical. TRUE for redownloading and overwriting previously
#'   downloaded files of the same names.
#'
#' @export
#' @importFrom curl curl_fetch_disk
#'
#' @examples
#' my_path <- "~/my/directory/"
#' # Download data from radars "bejab" and "bewid", even if previously
#' # downloaded (overwrite = TRUE). Will successfully download 2016-10 files,
#' # but show 404 error for 2016-11 files (as these are not available).
#' \dontrun{
#' download_vpfiles(
#'   date_min = "2016-10-01",
#'   date_max = "2016-11-30",
#'   radar = c("bejab", "bewid"),
#'   directory = my_path,
#'   overwrite = TRUE
#' )
#' }
download_vpfiles <- function(date_min, date_max, radars, directory = ".",
                             overwrite = FALSE) {
  # Stop if radar codes don't contain exactly 5 characters
  wrong_codes <- radars[nchar(radars) != 5]
  if (length(wrong_codes) > 0) {
    stop("Radar codes should contain exactly 5 letters: ",
         paste(wrong_codes, collapse = ", "))
  }

  # Split 5 letter radar codes into form be_jab
  ra_dars <- paste(substring(radars, 1, 2), substring(radars, 3, 5),
                       sep = "_")

  # Create series of year/month based on date_min/max: 2016/10, 2016/11, 2016/12
  dates <- seq(
    as.Date(date_min, tz = NULL),
    as.Date(date_max, tz = NULL),
    by = "months"
  )
  year_months <- format(dates, "%Y/%m")

  # Expand to series of radar/year/month: be_jab/2016/10, be_wid/2016/10, ...
  radar_year_months <- apply(expand.grid(ra_dars, year_months), 1, paste,
                             collapse = "/")

  # Set base url of data repository
  base_url <- "https://lw-enram.s3-eu-west-1.amazonaws.com"

  # Start download and unzipping
  message(paste("Downloading data from", base_url))

  for (radar_year_month in radar_year_months) {
    # Create filename of form bejab201610.zip (removing _ and /)
    file_name <- paste0(gsub("/", "", gsub("_", "", radar_year_month)), ".zip")
    # Create filepath of form directory/bejab201610.zip
    file_path <- file.path(directory, file_name)
    # Create url of form base_url/be/jab/2016/bejab201610.zip (removing month)
    url <- paste(base_url, gsub("_", "/",
                substring(radar_year_month, 1, nchar(radar_year_month) - 3)),
                file_name,
                sep = "/")
    # Create local unzip directory of form directory/bejab/2016/10
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
