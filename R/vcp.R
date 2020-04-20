#' Extract a volume coverage pattern table
#'
#' @param x Either a pvol or scan for which the VCP should be created.
#' @param add_params A logical wether a column with parameters be added.
#' @param select A charcter vector which the column names that should be returned when NULL all attributes are returned
#' @param ... Currently not used
#'
#' This function tabulates the attributes of one scan or all scans of a pvol. Attributes that have a length longer then one are presented as a list column.
#' By default the function returns a limited set of column to keep the output clear.
#' It is important to note thad attributes of the polar volume can contain additional information on processing that is not included in the resulting table.
#'
#' @export
#'
#' @examples
#' data(example_scan)
#' vcp(example_scan)
#'
#' pvolfilepath <- system.file("extdata", "volume.h5", package = "bioRad")
#' example_pvol <- read_pvolfile(pvolfilepaht)
#' vcp(example_pvol)
vcp <-
  function(x,
           add_params = T,
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
    assert_that(inherits(x, "scan") | inherits(x, "pvol"))
    assert_that(is.flag(add_params))
    assert_that(is.character(select) | is.null(select))
    if (class(x) == "pvol") {
      df <-
        do.call(
          "rbind",
          lapply(
            x$scans,
            vcp,
            add_params = add_params,
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
    if (add_params) {
      df$params <- list(names(x$params))
    }
    return(df)
  }
