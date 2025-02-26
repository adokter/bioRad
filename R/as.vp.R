#' Convert a dataframe into a vp object
#'
#' @param data a dataframe created from a VPTS CSV file
#' @returns a bioRad vp object
#' @examples
#' # load vp data as a data.frame:
#' df <- as.data.frame(example_vp)
#' # convert the data.frame to a vp object:
#' as.vp(df)
#' @export
as.vp <- function(data) {
  assertthat::assert_that(inherits(data,"data.frame"))

  vpts <- as.vpts(data)
  
  assertthat::assert_that(length(vpts$datetime) == 1, msg="multiple timestamps found, data is not a single vertical profile")
  
  vpts_to_vp(vpts)
}
