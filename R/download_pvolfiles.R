#' Download polar volume (\code{pvol}) files from the NEXRAD archive
#'
#' Download a selection of polar volume (\code{pvol}) files from the
#' \href{https://registry.opendata.aws/noaa-nexrad/}{NEXRAD Level II archive data}.
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
#'
#' @examples
#' # Download data from radar "KBBX", even if previously downloaded
#' # (overwrite = TRUE).
#' \dontrun{
#' dir.create("~/bioRad_tmp_files")
#' download_pvolfiles(
#'   date_min = "2016-10-02 20:00",
#'   date_max = "2016-10-03 06:00",
#'   radar = "KBBX",
#'   directory = "~/bioRad_tmp_files",
#'   overwrite = TRUE
#' )
download_pvolfiles <- function(date_min, date_max, radar, directory = ".",
                             overwrite = FALSE, bucket = "noaa-nexrad-level2") {

  # Ensure directory exists
  assert_that(is.dir(directory))

  # Stop if radar codes are not exactly 5 characters
  assert_that(is.character(radar))
  assert_that(length(radar)==1, msg = paste0("radar is not of length 1"))
  possible_radar = c('KABR','KABX','KAKQ','KAMA','KAMX','KAPX','KARX','KATX','KBBX','KBGM','KBHX','KBIS','KBLX','KBMX','KBOX','KBRO','KBUF','KBYX','KCAE','KCBW','KCBX','KCCX','KCLE','KCLX','KCRP','KCXX','KCYS','KDAX','KDDC','KDFX','KDGX','KDIX','KDLH','KDMX','KDOX','KDTX','KDVN','KDYX','KEAX','KEMX','KENX','KEOX','KEPZ','KESX','KEVX','KEWX','KEYX','KFCX','KFDR','KFDX','KFFC','KFSD','KFSX','KFTG','KFWS','KGGW','KGJX','KGLD','KGRB','KGRK','KGRR','KGSP','KGWX','KGYX','KHDX','KHGX','KHNX','KHPX','KHTX','KICT','KICX','KILN','KILX','KIND','KINX','KIWA','KIWX','KJAN','KJAX','KJGX','KJKL','KLBB','KLCH','KLGX','KLIX','KLNX','KLOT','KLRX','KLSX','KLTX','KLVX','KLWX','KLZK','KMAF','KMAX','KMBX','KMHX','KMKX','KMLB','KMOB','KMPX','KMQT','KMRX','KMSX','KMTX','KMUX','KMVX','KMXX','KNKX','KNQA','KOAX','KOHX','KOKX','KOTX','KOUN','KPAH','KPBZ','KPDT','KPOE','KPUX','KRAX','KRGX','KRIW','KRLX','KRMX','KRTX','KSFX','KSGF','KSHV','KSJT','KSOX','KSRX','KTBW','KTFX','KTLH','KTLX','KTWX','KTYX','KUDX','KUEX','KVAX','KVBX','KVNX','KVTX','KVWX','KYUX')
  assert_that(radar %in% possible_radar, msg = paste0("radar ", radar, " doesn't exist"))

  # Stop if dates are not a string
  assert_that(is.string(date_min))
  assert_that(is.string(date_max))

  # Stop if dates are not in YYYY-MM-DD format:
  check_date_format(date_min, "%Y-%m-%d %H:%M")
  check_date_format(date_max, "%Y-%m-%d %H:%M")

  # Stop if overwrite is not a logical
  assert_that(is.logical(overwrite), msg='overwrite is not a logical')

  # Create series of unique days yyyy-mm-dd based on date_min/max:
  dates <- seq(
    as.Date(substring(date_min, 1, 10), tz = NULL),
    as.Date(substring(date_max, 1, 10), tz = NULL),
    by = "days"
  )
  year_months <- format(dates, "%Y/%m")

  # Start download and unzipping
  message(paste0("Downloading data from ",bucket))

  for (d in dates) {
    # Create filename of format bejab201610.zip (removing _ and /)
    file_name <- paste0(gsub("/", "", gsub("_", "", radar_year_month)), ".zip")
    # Create filepath of format directory/bejab201610.zip
    file_path <- file.path(directory, file_name)

    # set prefix
    prefix = paste(gsub("-","/",d) ,radar, "" , sep="/")

    # Get bucket matching the request
    bucket_df = aws.s3::get_bucket_df(bucket = bucket, prefix = prefix)

    # filter bucket with exact date
    isWithin = sapply(bucket_df$Key,function(x){
      dd = strptime(substring(x,21,35), format = "%Y%m%d_%H%M%S")
      dd>=strptime(date_min, format = "%Y-%m-%d %H:%M") & dd<=strptime(date_max, format = "%Y-%m-%d %H:%M")
    })
    bucket_df = bucket_df[isWithin, ]

    # create progresbar
    message(paste0("Downloading pvol for ",prefix))
    pb = txtProgressBar(min = 0, max = nrow(bucket_df), initial = 0, style = 3)

    for (row in 1:nrow(bucket_df)) {

      # Create url of format base_url/be/jab/2016/bejab201610.zip (removing month)
      aws.s3::save_object(object = bucket_df$Key[row],
                          bucket = bucket,
                          file = paste(directory, bucket_df$Key[row], sep = "/"),
                          overwite = overwrite)

      setTxtProgressBar(pb,row)
    }

  }
}



#' Check if character date is in specific format
#'
#' @param date character. Character representation of a date, e.g.
#'   \code{"2018-12-13"}.
#' @param format character. strptime format the date should have, e.g.
#'   \code{"\%Y-\%m-\%d"}
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
