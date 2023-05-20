#' @details
#' To get started, see:
#'
#' * Dokter et al. (2019) \doi{10.1111/ecog.04028}: a paper describing the
#'   package.
#' * [bioRad
#'   vignette](https://adriaandokter.com/bioRad/articles/bioRad.html): an
#'   introduction to bioRad's main functionalities.
#' * [Function
#'   reference](https://adriaandokter.com/bioRad/reference/index.html): an
#'   overview of all bioRad functions.
#'
#' @keywords internal
#'
#' @import stats
#' @import rhdf5
#' @import methods
#' @import graphics
#' @import rgdal
#' @import sp
#' @import utils
#' @importFrom raster rasterToPoints
#' @importFrom raster raster
#' @importFrom raster rasterize
#' @importFrom raster values
#' @importFrom raster values<-
#' @importFrom raster extent
#' @importFrom raster crs
#' @importFrom readr parse_datetime
#' @importFrom grDevices col2rgb colorRampPalette rgb
#' @importFrom maptools crepuscule
#' @importFrom tidyr fill
#' @importFrom dplyr %>% .data
#'
"_PACKAGE"
