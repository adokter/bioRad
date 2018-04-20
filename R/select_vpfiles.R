#' Select vertical profiles files (\code{vp}) from computer
#'
#' Collect a list of vp file names within a directory that comply to the given
#' country, radar and date range combination
#'
#' @param path Main path to look into recusively.
#' @param start_date ISO format date as start of the vp file query.
#' @param end_date ISO format date as end of the vp file query.
#' @param country Character vector with two letter country shortcuts.
#' @param radar Character vector with three letter radar sindicators. This
#' can be defined independently from the countries named.
#'
#' @return Character list of filenames that comply to the given
#' radar/country and date range query
#'
#' @export
#' @importFrom lubridate as_date
#'
#' @examples
#' my_path <- "~/my/directory/"
#' select_vpfiles(my_path, "2016-10-01", "2017-01-31", c("be"))
select_vpfiles <- function(path, start_date, end_date,
                           country = NULL, radar = NULL) {
  if (is.null(country)) {country <- "([a-z]{2})"}
  if (is.null(radar)) {radar <- "([a-z]{3})"}

  # create period of dates to check for
  start <- as_date(start_date, tz = NULL)
  end <- as_date(end_date, tz = NULL)
  dates_to_check <- seq(start, end, by = 'days')

  filelist <- dir(path, recursive = TRUE)

  datestring_to_check <- format(dates_to_check, "%Y%m%d")
  countryradar <- apply(expand.grid(country, radar), 1, paste,collapse = "")
  countryradardate <- apply(expand.grid(countryradar, "_vp_",
                                        datestring_to_check), 1, paste,
                            collapse = "")
  match_filenames(filelist, paste(countryradardate, collapse = "|"))

}

#' Match a set of regex expression to a list of files
#'
#' Match a set of regex expression to a list of files and return those filenames
#' that comply to any of the provided regex expressions. This function basically
#' wraps a grep to make it working on vectors by combining the vector of
#' regex options as possible options
#'
#' @param filelist Character list of filenames/filepaths.
#' @param regexlist Character list of regex expressions to which the file names
#' should comply.
#'
#' @keywords internal
#'
#' @return Character subset of filenames from the filelist that comply to any
#' of the provided regex expressions
match_filenames <- function(filelist, regexlist) {
  grep(paste(regexlist, collapse = "|"), filelist, value = TRUE)
}
