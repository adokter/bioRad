#' Select vertical profile (`vp`) files from computer
#'
#' Create a list of vertical profile (`vp`) files from a local directory
#' that match a specific date and radar range. Files are selected based on their
#' file name (not directory structure), which should be of format
#' `radar_vp_yyyymmdd*.*`, such as `bewid_vp_20171123T1900Z_0x5.h5`.
#'
#' @param date_min character. YYYY-MM-DD start date of file selection.
#' @param date_max character. YYYY-MM-DD end date of file selection.
#' @param radars character (vector). 5-letter country/radar code(s) (e.g.
#'   `bejab`) of radars to include in file selection.
#' @param directory character. Path to local directory where files should be
#'   looked for.
#'
#' @return Character vector of file paths that comply to the given date and
#'   radar range.
#'
#' @export
#'
#' @seealso download_vpfiles
#'
#' @examples
#' select_vpfiles(
#'   date_min = "2016-10-03",
#'   date_max = "2016-10-05",
#'   radars = "bejab",
#'   directory = "my_data"
#' )
select_vpfiles <- function(date_min = NULL, date_max = NULL, radars = NULL,
                           directory = ".") {
  # Stop if radar codes are not exactly 5 characters
  check_radar_codes(radars)

  # If radars not defined, create regex for any 5 letters lowercase code
  if (is.null(radars)) {
    radars <- "([a-z]{5})"
  }

  # Create series of dates based on date_min/max: 20161001, 20161002, ...
  # or regex for any yyyymmdd date
  if (!is.null(date_min) && !is.null(date_max)) {
    # Stop if dates are not in YYYY-MM-DD format:
    check_date_format(date_min, "%Y-%m-%d")
    check_date_format(date_max, "%Y-%m-%d")

    dates <- seq(
      as.Date(date_min, tz = NULL),
      as.Date(date_max, tz = NULL),
      by = "days"
    )
    dates <- format(dates, "%Y%m%d")
  } else {
    dates <- c("(19|20)\\d\\d(0[1-9]|1[012])(0[1-9]|[12][0-9]|3[01])")
  }

  # Expand to series of radar_vp_yyyymmdd: bejab_vp_20161001, bejab_vp_20161002
  radar_dates <- apply(expand.grid(radars, "_vp_", dates), 1, paste,
    collapse = ""
  )

  # Create list of all files in target directory
  all_files <- dir(directory, recursive = TRUE)

  # Search for radar_dates filenames in all file names
  matched_files <- match_filenames(all_files, paste(radar_dates, collapse = "|"))

  # Append target directory to file paths
  file.path(directory, matched_files)
}

#' Match a set of regular expressions to a list of files
#'
#' Match a set of regular expressions to a list of files and return those
#' filenames that comply to any of the provided regular expressions. This
#' function basically wraps a grep to make it work on vectors by combining the
#' vector of regex options as possible options.
#'
#' @param file_list character vector. Haystack of filenames/filepaths.
#' @param regex_list character vector. Needle of regular expressions to which
#'   filenames should comply.
#'
#' @return character vector. Subset of filenames from the file_list that comply
#'   to the provided regular expressions in regex_list.
#' @keywords internal
match_filenames <- function(file_list, regex_list) {
  grep(paste(regex_list, collapse = "|"), file_list, value = TRUE)
}
