#' Check is a specific URL is existing and providing a return message
#'
#' @param url char weblink to a potentially existing webpage
#'
#' @return z with a lenght > 1 if the URL is existing and downloading would be
#' possible
#'
#' @export
#' @keywords internal
#' @importFrom RCurl getBinaryURL
url_existence <- function(url) {
  z <- ""
  tryCatch(z <- getBinaryURL(url, failonerror = TRUE) ,
           error = function(e) {print(paste("no data available at URL",  url))})
  return(z)
}

#' Download a set of vp bird profiles from ENRAM repository
#'
#' Download a set of vp bird profiles from the ENRAM repository.
#' These are stored within monthly available
#' zip folders. This function downloads and unzips them at a user defined location.
#' Check \href{http://enram.github.io/data-repository/}{http://enram.github.io/data-repository/} for an overview
#' of available data.
#'
#' @param start_date ISO fomat date indicating the first date to download files from
#' @param end_date ISO fomat date indicating the last date to download files from
#' @param country char vector with two letter country shortcuts
#' @param radar char vector with three letter radar sindicators. Make sure the
#' radars selected are in accordance to the country selection
#' @param localpath char defining the location to store the downloaded zip
#' folders and unzip into the default folder structure
#'
#' @export
#' @importFrom lubridate as_date
#' @importFrom curl curl_download
#' @examples
#' my_path <- "~/my/directory/"
#' \dontrun{download_vp("2016-10-01", "2016-11-30", c("be"), c("jab", "wid"), localpath = my_path)}
download_vp <- function(start_date, end_date, country, radar, localpath = ".") {

  # create date range set of potential downloadable zip files (if all data
  # would exist)
  start <- as_date(start_date, tz = NULL)
  end <- as_date(end_date, tz = NULL)
  dates_to_check <- seq(start, end, by = 'months')

  # ZIP-file format preparation
  countryradar <- apply(expand.grid(country, radar), 1, paste, collapse = "")
  datestring_to_check <- format(dates_to_check, "%Y%m")
  countryradardate <- apply(expand.grid(countryradar,
                                        datestring_to_check, ".zip"), 1,
                            paste, collapse = "")
  # PATH-format
  countryradarpath <- apply(expand.grid(country, radar,
                                        format(dates_to_check, "%Y")), 1,
                            paste,
                            collapse = "/")
  # PATH-format as it will be represented locally
  countryradarlocalpath <- apply(expand.grid(country, radar,
                                             format(dates_to_check, "%Y/%m")),
                                 1,
                                 paste,
                                 collapse = "/")
  countryradarlocalpath <- file.path(localpath, countryradarlocalpath)

  # combine base path (S3 location) with the potential data URLS
  base_url <- "https://lw-enram.s3-eu-west-1.amazonaws.com"
  urls <- paste(base_url, "/", countryradarpath, "/",
                countryradardate, sep = "")

  # Attempt download at predefined location
  for (i in 1:length(urls)) {
    z <- url_existence(urls[i])
    if (length(z) > 1) {
      print(paste("Downloading file", countryradardate[i]))
      curl_download(urls[i], file.path(localpath, countryradardate[i]),
                    quiet = FALSE)

      # Unzip the downloaded archive to the common file structure
      unzip(file.path(localpath, countryradardate[i]),
            exdir = countryradarlocalpath[i])
    }

  }
}

#' Match a set of regex expression to a list of files
#'
#' Match a set of regex expression to a list of files and return those filenames
#' that comply to any of the provided regex expressions. This function basically
#' wraps a grep to make it working on vectors by combining the vector of
#' regex options as possible options
#'
#' @param filelist char list of filenames/filepaths
#' @param regexlist char list of regex expressions to which the file names
#' should comply
#' @keywords internal
#' @return char subset of filenames from the filelist that comply to any of the
#' provided regex expressions
#'
#' @export
match_filenames <- function(filelist, regexlist) {
  grep(paste(regexlist, collapse = "|"), filelist, value = TRUE)
}

#' Collect a list of vp file names
#'
#' Collect a list of vp file names within a directory that comply to the given
#' country, radar and date range combination
#'
#' @param path main path to look into recusively
#' @param start_date ISO format date as start of the vp file query
#' @param end_date ISO format date as end of the vp file query
#' @param country char vector with two letter country shortcuts
#' @param radar char vector with three letter radar sindicators. This can be
#' defined independently from the countries named.
#'
#' @return char list of filenames that comply to the given radar/country and
#' date range query
#'
#' @export
#' @importFrom lubridate as_date
#'
#' @examples
#' my_path <- "~/my/directory/"
#' retrieve_vp_paths(my_path, "2016-10-01", "2017-01-31", c("be"))
retrieve_vp_paths <- function(path, start_date, end_date,
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
  countryradardate <- apply(expand.grid(countryradar, "_vp_", datestring_to_check), 1, paste, collapse = "")
  match_filenames(filelist, paste(countryradardate, collapse = "|"))

}
