#' Extract a volume coverage pattern table with all attributes
#'
#' @param x Either a pvol or scan for which the table should be created.
#' @param select A character vector which the column names that should be returned when NULL all attributes are to be returned
#' @param ... Currently not used
#'
#' This function tabulates the attributes of one scan or all scans of a pvol.
#' Attributes that have a length longer then one are presented as a list column.
#' By default the function returns a limited set of columns to keep the output clear.
#' It is important to note that attributes of the full polar volume can contain additional information on processing that is not included in the resulting table.
#' This function only tabulates attributes of the scans.
#'
#' @export
#'
#' @examples
#' data(example_scan)
#' attribute_table(example_scan)
#'
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' example_pvol <- read_pvolfile(pvolfile)
#' attribute_table(example_pvol)
attribute_table <-
  function(x,
           select = c(
             "how.lowprf",
             "how.midprf",
             "how.highprf",
             "where.elangle",
             "where.nbins",
             "where.nrays",
             "where.rscale",
             "how.NI"
           ),
           ...) {
    assertthat::assert_that(inherits(x, "scan") | inherits(x, "pvol"),
                            msg = "`x` must be a pvol or scan object")
    assertthat::assert_that(is.character(select) | is.null(select),
                            msg = "when provided, `select` must be a character vector")
    if (inherits(x, "pvol")) {
      df <-
        do.call(
          "rbind",
          lapply(
            x$scans,
            attribute_table,
            select = select,
            ...
          )
        )
      return(df)
    }
    t <- unlist(x$attributes, F)
    g <- lapply(t, function(x) {
      ifelse(length(x) != 1, (list(x)), x)
    })
    df <- structure(g, class = "data.frame", row.names = "")
    if (!is.null(select)) {
      df <- df[, colnames(df) %in% select, drop = F]
    }
    df$param <- list(names(x$params))

    return(df)
  }
