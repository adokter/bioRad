#' @details
#' bioRad aims to standardize methods for extracting and reporting biological
#' signals from weather radars. It provides functionality to access low‐level
#' radar data, process these data into meaningful biological information on
#' animal speeds and directions at different altitudes in the atmosphere,
#' visualize these biological extractions, and calculate further summary
#' statistics.
#'
#' To get started, see:
#'
#' \itemize{
#'   \item \href{https://doi.org/10.1111/ecog.04028}{Dokter et al. (2018)}: a
#'   paper describing the package.
#'   \item \href{https://adokter.github.io/bioRad/articles/bioRad.html}{bioRad
#'   vignette}: an introduction to bioRad's main functionalities.
#'   \item \href{https://adokter.github.io/bioRad/reference/index.html}{Function
#'   reference}: an overview of all bioRad functions.
#' }
#'
#' @keywords internal
#'
#' @import stats
#' @import rhdf5
#' @import fields
#' @import methods
#' @import graphics
#' @import ggplot2
#' @import ggmap
#' @import rgdal
#' @import sp
#' @import utils
#' @importFrom raster rasterToPoints
#' @importFrom raster raster
#' @importFrom curl curl_download
#' @importFrom grDevices col2rgb colorRampPalette rgb
#' @importFrom maptools crepuscule
#'
"_PACKAGE"
