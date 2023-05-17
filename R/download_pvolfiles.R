#' Download polar volume (`pvol`) files from the NEXRAD archive
#'
#' Download a selection of polar volume (`pvol`) files from the
#' [NEXRAD Level II archive
#'  data](https://registry.opendata.aws/noaa-nexrad/).
#'
#' @param date_min POSIXct. Start date of file selection. If no timezone are
#' provided, it will be assumed to be UTC.
#' @param date_max POSIXct. End date of file selection.If no timezone are
#' provided, it will be assumed to be UTC.
#' @param radar character (vector). 4-letter radar code(s) (e.g. "KAMA")
#' @param directory character. Path to local directory where files should be
#'   downloaded
#' @param overwrite logical. TRUE for re-downloading and overwriting previously
#'   downloaded files of the same names.
#' @param bucket character. Bucket name to use.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dir.create("~/bioRad_tmp_files")
#' download_pvolfiles(
#'   date_min = as.POSIXct("2016-10-02 20:00", tz = "UTC"),
#'   date_max = as.POSIXct("2016-10-02 20:05", tz = "UTC"),
#'   radar = "KBBX",
#'   directory = "~/bioRad_tmp_files",
#'   overwrite = TRUE
#' )
#' }
download_pvolfiles <- function(date_min, date_max, radar,
                               directory = ".", overwrite = FALSE,
                               bucket = "noaa-nexrad-level2") {

  # Ensure directory exists
  assert_that(is.dir(directory))

  # Stop if radar codes are not exactly 5 characters
  assert_that(is.character(radar))
  assert_that(length(radar) == 1, msg = paste0("radar is not of length 1"))

  # Stop if dates are not date and not
  assert_that(lubridate::is.POSIXt(date_min), msg = "date_min is not a date")
  assert_that(lubridate::is.POSIXt(date_max), msg = "date_max is not a date")
  assert_that(date_min <= date_max,
              msg = "date_max is not greater or equal to date_min")

  # Stop if overwrite is not a logical
  assert_that(is.logical(overwrite), msg = "overwrite is not a logical")

  # Change timezone
  if (attr(date_min, "tzone") == "") {
    date_min <- as.POSIXct(format(date_min), tz = "UTC")
  }
  if (attr(date_max, "tzone") == "") {
    date_max <- as.POSIXct(format(date_max), tz = "UTC")
  }
  attr(date_min, "tzone") <- "UTC"
  attr(date_max, "tzone") <- "UTC"

  # Create series of unique days yyyy-mm-dd based on date_min/max:
  dates <- seq(as.Date(date_min), as.Date(date_max), by = "days")

  # Start download and unzipping
  message(paste0(
    "Downloading data from ", bucket, " for radar ", radar,
    " spanning over ", length(dates), " days"
  ))

  for (i_d in seq_len(length(dates))) {

    # set prefix
    prefix <- paste(format(dates[i_d], "%Y/%m/%d"), radar, "", sep = "/")

    # Get bucket matching the request
    tryCatch(
      {
        bucket_df <- aws.s3::get_bucket_df(bucket = bucket, prefix = prefix)
      },
      error = function(cond) {
        assert_that(aws.s3::bucket_exists(bucket = bucket),
          msg = paste0("The bucket ", bucket, "does not exist")
        )
      }
    )

    # Check that bucket_df is not empty
    if (nrow(bucket_df) == 0) {
      # Check if date is correct
      prefix_tmp <- paste(gsub("-", "/", dates[i_d]), sep = "/")
      assert_that(not_empty(
        aws.s3::get_bucket_df(bucket = bucket, prefix = prefix_tmp, max = 1)
      ),
      msg = paste0(
        "No data availble on the ", dates[i_d],
        ". Please check data availability for this date."
      )
      )
      assert_that(not_empty(
        aws.s3::get_bucket_df(bucket = bucket, prefix = prefix, max = 1)
      ),
      msg = paste0(
        "No data available for ", radar, " on the ", dates[i_d],
        ". Check radar code and data availability on",
        " https://noaa-nexrad-level2.s3.amazonaws.com/index.html"
      )
      )
    }

    # filter bucket with exact date
    isWithin <- sapply(bucket_df$Key, function(x) {
      dd <- as.POSIXct(substring(x, 21, 35),
        format = "%Y%m%d_%H%M%S",
        tz = "UTC"
      )
      dd >= date_min &
        dd <= date_max
    })
    bucket_df <- bucket_df[isWithin, ]

    # throw out occasional NA keys, see e.g. 2015/03/01/KEPZ/
    bucket_df %>% dplyr::filter(!is.na(.data$Key)) -> bucket_df

    assert_that(nrow(bucket_df) > 0,
      msg = paste0(
        "No data available for ", radar, " on the ", dates[i_d],
        "within the selected datetime range. Check radar code and data availability on",
        " https://noaa-nexrad-level2.s3.amazonaws.com/index.html"
      ))

    # create progress bar
    message(paste0("\nDownloading pvol for ", prefix))
    pb <- txtProgressBar(min = 0, max = nrow(bucket_df), initial = 0, style = 3)

    for (row in 1:nrow(bucket_df)) {
      # Save file
      aws.s3::save_object(
        object = bucket_df$Key[row],
        bucket = bucket,
        file = paste(directory, bucket_df$Key[row], sep = "/"),
        overwite = overwrite
      )
      setTxtProgressBar(pb, row)
    }
  }
}
