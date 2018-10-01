#' Check if a specific URL exists
#'
#' @param url Weblink (char) to a potentially existing webpage.
#'
#' @return z With a length > 1 if the URL is existing and downloading would be
#' possible.
#'
#' @keywords internal
#' @importFrom RCurl getBinaryURL
check_url_existence <- function(url) {
  z <- ""
  tryCatch(z <- getBinaryURL(url, failonerror = TRUE),
    error = function(e) {
      print(paste("no data available at URL", url))
    }
  )
  return(z)
}

#' Download vertical profile files (\code{vp}) from the ENRAM data repository
#'
#' Download a set of vp bird profiles from the ENRAM repository. These are
#' stored within monthly available zip folders. This function downloads and
#' unzips them at a user defined location. Check
#' \href{http://enram.github.io/data-repository/}{http://enram.github.io/data-repository/}
#' for an overview of available data.
#'
#' @param date_min ISO fomat date indicating the first date to download
#' files from.
#' @param date_max ISO fomat date indicating the last date to download
#' files from
#' @param country Char vector with two letter country shortcuts.
#' @param radar Char vector with three letter radar sindicators. Make sure the
#' radars selected are in accordance to the country selection
#' @param directory Char defining the location to store the downloaded zip
#' folders and unzip into the default folder structure
#'
#' @export
#' @importFrom lubridate as_date floor_date
#' @importFrom curl curl_download
#'
#' @examples
#' my_path <- "~/my/directory/"
#' \dontrun{download_vpfiles("2016-10-01", "2016-11-30", c("be"),
#' c("jab", "wid"), directory = my_path)}
download_vpfiles <- function(date_min, date_max, country, radar,
                             directory = ".") {
  # create date range set of potential downloadable zip files (if all data
  # would exist)
  start <- floor_date(as_date(date_min, tz = NULL), "month")
  end <- floor_date(as_date(date_max, tz = NULL), "month")
  dates_to_check <- seq(start, end, by = "months")

  # ZIP-file format preparation
  countryradar <- apply(expand.grid(country, radar), 1, paste, collapse = "")
  datestring_to_check <- format(dates_to_check, "%Y%m")
  countryradardate <- apply(expand.grid(
    countryradar,
    datestring_to_check, ".zip"
  ), 1,
  paste,
  collapse = ""
  )
  # PATH-format
  countryradarpath <- apply(expand.grid(
    country, radar,
    format(dates_to_check, "%Y")
  ), 1,
  paste,
  collapse = "/"
  )
  # PATH-format as it will be represented locally
  countryradardirectory <- apply(expand.grid(
    country, radar,
    format(dates_to_check, "%Y/%m")
  ),
  1,
  paste,
  collapse = "/"
  )
  countryradardirectory <- file.path(directory, countryradardirectory)

  # combine base path (S3 location) with the potential data URLS
  base_url <- "https://lw-enram.s3-eu-west-1.amazonaws.com"
  urls <- paste(base_url, "/", countryradarpath, "/",
    countryradardate,
    sep = ""
  )

  # Attempt download at predefined location
  for (i in 1:length(urls)) {
    z <- check_url_existence(urls[i])
    if (length(z) > 1) {
      print(paste("Downloading file", countryradardate[i]))
      curl_download(urls[i], file.path(directory, countryradardate[i]),
        quiet = FALSE
      )

      # Unzip the downloaded archive to the common file structure
      unzip(file.path(directory, countryradardate[i]),
        exdir = countryradardirectory[i]
      )
    }
  }
}
