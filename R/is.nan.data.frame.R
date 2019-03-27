#' Identify \code{NaN} in a dataframe.
#'
#' Identify cells with \code{NaN} in a data frame. Improve on the defeault
#' \code{is.nan()} function, which only works on vectors, by allowing
#' data frames as input.
#'
#' @param x A data frame to be tested.
#' @return A matrix of the same dimension as \code{x}, with TRUE/FALSE values
#' for whether each cell in the original data frame is a number or not.
#' \code{NaN} means 'Not a Number'.
#' @keywords internal
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
