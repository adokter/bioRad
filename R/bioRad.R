#' @details
#' To get started, see:
#'
#' \itemize{
#'   \item Dokter et al. (2019) \doi{10.1111/ecog.04028}: a
#'   paper describing the package.
#'   \item \href{https://adriaandokter.com/bioRad/articles/bioRad.html}{bioRad
#'   vignette}: an introduction to bioRad's main functionalities.
#'   \item \href{https://adriaandokter.com/bioRad/reference/index.html}{Function
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
#' @import glue
#' @import rgdal
#' @import sp
#' @import utils
#' @import assertthat
#' @importFrom dplyr mutate pull bind_rows left_join join_by
#' @importFrom raster rasterToPoints
#' @importFrom raster raster
#' @importFrom raster rasterize
#' @importFrom raster values
#' @importFrom raster values<-
#' @importFrom raster extent
#' @importFrom raster crs
#' @importFrom readr parse_datetime
#' @importFrom curl curl_download
#' @importFrom grDevices col2rgb colorRampPalette rgb
#' @importFrom maptools crepuscule
#' @importFrom tidyr fill
#'
"_PACKAGE"
