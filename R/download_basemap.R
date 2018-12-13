#' Download a basemap for \code{map(ppi)}
#'
#' Downloads a Google Maps, OpenStreetMap, Stamen Maps or Naver Map base
#' layer map using \link[ggmap]{get_map}.
#'
#' @param x An object of class \code{ppi}.
#' @param zoom Zoom level (optional), see \link[ggmap]{get_map}. An integer
#'   from 3 (continent) to 21 (building). By default the zoom level matching the
#'   ppi extent is selected automatically.
#' @param alpha Transparency of the basemap (0-1).
#' @param verbose Logical, whether to print information to console.
#' @param source String identifying which map service should be used: "google", "osm" or "stamen"
#' @param ... Arguments to pass to \link[ggmap]{get_map} function. Note arguments \code{maptype} and \code{source}
#' for selection of different types of basemaps.s
#'
#' @export
#'
#' @examples
#' # load an example scan:
#' data(example_scan)
#' # print summary info for the scan:
#' example_scan
#' # make ppi for the scan
#' ppi <- project_as_ppi(example_scan)
#' # grab a basemap that matches the extent of the ppi:
#' \dontrun{
#' basemap <- download_basemap(ppi)
#' # map the reflectivity quantity of the ppi onto the basemap:
#' map(ppi, map = basemap, param = "DBZH")
#' # download a different type of basemap, e.g. satellite imagery:
#' # see get_map() in ggmap library for full documentation of options
#' basemap <- download_basemap(ppi, maptype = "satellite")
#' # map the radial velocities onto the satellite imagery:
#' map(ppi, map = basemap, param = "VRADH")
#' }
download_basemap <- function(x, verbose = TRUE, zoom, alpha = 1, source = "stamen", ...) {
  stopifnot(inherits(x, "ppi"))

  if(compareVersion(as.character(packageVersion("ggmap")),"2.7.904")<0) stop("version of package ggmap should be >= 2.7.904, visit https://github.com/dkahle/ggmap for upgrade instructions")

  if (source != "google") {
    location <- c(left = x$geo$bbox["lon", "min"], bottom = x$geo$bbox["lat", "min"], right = x$geo$bbox["lon", "max"], top = x$geo$bbox["lat", "max"])
  } else {
    location <- c(lon = mean(x$geo$bbox["lon", ]), lat = mean(x$geo$bbox["lat", ]))
  }


  if (!missing(zoom)) {
    if (!is.numeric(zoom)) {
      stop("zoom should be a numeric integer")
    }
  }
  # check size of ppi and determine zoom
  if (missing(zoom)) {
    use_zoom <- calc_zoom(x$geo$bbox["lon", ], x$geo$bbox["lat", ])
  } else {
    use_zoom <- zoom
  }

  if (verbose) {
    cat("Downloading zoom =", use_zoom, "...\n")
  }
  map <- get_map(
    location = location,
    zoom = use_zoom,
    source = source,
    ...
  )
  bboxmap <- attributes(map)$bb

  if ((x$geo$bbox["lon", "max"] - x$geo$bbox["lon", "min"] >
    bboxmap$ur.lon - bboxmap$ll.lon) ||
    (x$geo$bbox["lat", "max"] - x$geo$bbox["lat", "min"] >
      bboxmap$ur.lat - bboxmap$ll.lat)) {
    if (missing(zoom)) {
      if (verbose) {
        cat("Map too small, downloading zoom =", use_zoom - 1, "...\n")
      }
      map <- get_map(
        location = location,
        zoom = use_zoom - 1,
        source = source,
        ...
      )
      bboxmap <- attributes(map)$bb
      if ((x$geo$bbox["lon", "max"] - x$geo$bbox["lon", "min"] >
        bboxmap$ur.lon - bboxmap$ll.lon) ||
        (x$geo$bbox["lat", "max"] - x$geo$bbox["lat", "min"] >
          bboxmap$ur.lat - bboxmap$ll.lat)) {
        if (verbose) {
          cat("Map still too small, downloading zoom =", use_zoom - 2, "...\n")
        }
        map <- get_map(
          location = location,
          zoom = use_zoom - 2,
          source = source,
          ...
        )
      }
    } else {
      warning("Map is smaller than ppi bounding box.")
    }
  }
  attributes(map)$geo <- x$geo
  attributes(map)$ppi <- TRUE
  # add transparency
  add_color_transparency(map, alpha = alpha)
}
