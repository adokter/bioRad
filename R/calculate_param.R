#' Calculate a new parameter (`param`) for a scan (`scan`) or polar volume
#' (`pvol`)
#'
#' Calculates a new scan parameter (`param`) from a combination of existing scan
#' parameters. Useful for calculating quantities that are defined in terms of
#' other basic radar moments, like linear reflectivity eta, depolarization ratio
#' (Kilambi et al. 2018), or for applying clutter corrections (`CCORH`) to
#' uncorrected reflectivity moments (`TH`) as `TH + CCORH`.
#'
#' @param x A `pvol` or `scan` object.
#' @param ... An expression defining the new scan parameter in terms of existing
#'   scan parameters.
#'
#' @return An object of the same class as `x`, either a `pvol` or `scan`.
#'
#' @export
#'
#' @seealso
#' * [get_param()]
#'
#' @references
#' * Kilambi A, Fabry F, Meunier V (2018) A simple and effective method for
#' separating meteorological from nonmeteorological targets using
#' dual-polarization data. Journal of Atmospheric and Oceanic Technology 35, pp.
#' 1415–1424. \doi{10.1175/JTECH-D-17-0175.1}
#'
#' @examples
#' # Locate and read the polar volume example file
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' pvol <- read_pvolfile(pvolfile)
#'
#' # Calculate linear reflectivity ETA from reflectivity factor DBZH
#' radar_wavelength <- pvol$attributes$how$wavelength
#' pvol <- calculate_param(pvol, ETA = dbz_to_eta(DBZH, radar_wavelength))
#'
#' # Add depolarization ratio (DR) as a scan parameter (see Kilambi 2018)
#' pvol <- calculate_param(pvol, DR = 10 * log10((ZDR + 1 - 2 * ZDR^0.5 * RHOHV) /
#'   (ZDR + 1 + 2 * ZDR^0.5 * RHOHV)))
#'
#' # The function also works on scan and ppi objects
#' calculate_param(example_scan, DR = 10 * log10((ZDR + 1 - 2 * ZDR^0.5 * RHOHV) /
#'   (ZDR + 1 + 2 * ZDR^0.5 * RHOHV)))
#'
#' ppi <- project_as_ppi(example_scan)
#' calculate_param(ppi, exp(DBZH))
calculate_param <- function(x, ...) {
  UseMethod("calculate_param", x)
}

#' @describeIn calculate_param Calculate a new parameter (`param`) for all scans
#'   in a polar volume (`pvol`).
#' @export
calculate_param.pvol <- function(x, ...) {
  assert_that(is.pvol(x))
  x$scans <- do.call(lapply, list(x$scans, calculate_param.scan, substitute(list(...))))
  return(x)
}

#' @describeIn calculate_param Calculate a new parameter (`param`) for a plan
#'   position indicator (`ppi`).
#' @export
calculate_param.ppi <- function(x, ...) {
  assert_that(is.ppi(x))
  calc <- as.list(substitute(list(...)))[-1L]
  name <- names(calc)
  if (is.null(name)) {
    name <- rep("", length(calc))
  }
  for (i in seq_along(calc)) {
    newParam <- eval(nn <- (calc[[i]]), x$data@data)
    if ("" == (name[[i]])) {
      name[[i]] <- deparse(nn, width.cutoff = 250L)[1]
    }
    x$data@data[,name[[i]]]<-newParam
  }
  return(x)
}

#' @describeIn calculate_param Calculate a new parameter (`param`) for a scan
#'  (`scan`).
#' @export
calculate_param.scan <- function(x, ...) {
  assert_that(is.scan(x))
  # check if all parameters are equal
  attr_to_check<-c('class','radar','datetime','geo','dim')
  for(i in attr_to_check){
    lapply(x$params, function(param, i) assert_that(has_attr(param, i)),i=i)
    if(length(x$params)!=1)
      lapply(lapply(x$params[-1], attr, i), function(x,y) assert_that(are_equal(x,y)), y=attr(x$params[[1]], i))
  }
  if (as.character(as.list(substitute(...))[[1L]]) == "list") {
    calc <- as.list(substitute(...))[-1L]
  } else {
    calc <- as.list(substitute(list(...)))[-1L]
  }
  name <- names(calc)
  if (is.null(name)) {
    name <- rep("", length(calc))
  }
  for (i in seq_along(calc)) {
    newParam <- eval(nn <- (calc[[i]]), x$params)
    if ("" == (name[[i]])) {
      name[[i]] <- deparse(nn, width.cutoff = 250L)[1]
    }
    attr(newParam, "param") <- name[[i]]
    # reassign attributes if they are lost in operation
    for(j in attr_to_check) {
      if(!has_attr(newParam,j)) {
        attr(newParam,j) <- attr(x$params[[1]], j)
      }
    }
    x$params[[name[[i]]]] <- newParam
  }
  return(x)
}
