globalVariables(c("DBZH","VRADH","RHOHV"))
NULL

#' Create a velocity azimuth display (VAD) plot
#'
#' A velocity azimuth display plot visualized the radial velocity (`VRAD`) as a function of the azimuth from the radar.
#' Among others it can be used to asses the fit of the movement speeds in a vertical profile.
#' For example, when the movement is not uniform but rather in multiple directions or rotating this will be visible as deviations from the sine function.
#'
#' @param x A polar volume from which range gates are extracted.
#' @param ... Currently not used.
#' @param vp A vertical profile to annotate the VAD plot with the fit in the vertical profile
#' @param range The distance range in to filter the range gates with.
#'      If a `vp` is provided the range is taken from there and the argument `range` should not be provided.
#' @param height The height range to filter the range gates by.
#'      If only one value is provided next to a `vp` then the height bin intersection with this elevation is plotted.
#' @param range_gate_filter Optional filtering of the range gates. By default range gates are filtered for a `DBZH` less then 20 and a `RHOHV` less then 0.95.
#'      Alternative filters could be used to highlight specific effects.
#' @param point_geom The geom to visualize the range gates, the default is [ggplot2::geom_point()], in some cases this suffers from over plotting.
#'      Alternatives that avoid over plotting could be [ggplot2::geom_bin2d()] or [ggpointdensity::geom_pointdensity()].
#' @param point_geom_args Additional arguments to the `point_geom` function. For example, controlling the point size or alpha.
#' @param annotate A [glue::glue()] string that is used to annotate the plot with additional properties of the height bin of the `vp`.
#'      The string is evaluated using the columns from `as.data.frame(vp)`.
#'      Use `NULL` if no annotation is desired.
#' @param vp_color The color used for the `vp` annotations and line.
#' @param annotation_size The text size used for the annotation.
#'
#' @returns A [ggplot2::ggplot] object. Additional elements can be added using regular function in the `ggplot2` package.
#'
#' @export
#' @examples
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' example_pvol <- read_pvolfile(pvolfile)
#' # VAD plots can be created for polar volumes alone
#' vad(example_pvol,
#'   range = c(5000, 30000), height = c(200, 400)
#' )
#' # It is also possible to plot one height or more height bins from a `vp`.
#' # Many visual aspects can be controlled through the function arguments
#' # or through adding `ggplot`
#' vp <- calculate_vp(pvolfile)
#' vad(example_pvol,
#'   vp = vp,
#'   height = 400,
#'   point_geom_args=list(ggplot2::aes(color=ZDR))
#' ) + ggplot2::scale_color_gradient2()
#' vad(example_pvol,
#'   vp = vp, height=c(400,1200),
#'   point_geom=ggplot2::geom_bin2d,
#'   annotate="sdvvp: {round(sd_vvp,2)} [m/s]",
#' ) + ggplot2::scale_fill_viridis_c()
vad <- function(x, ...) {
  UseMethod("vad", x)
}
#' @rdname vad
#' @export
vad.pvol <- function(x, vp = NULL,..., range = NULL, height = NULL,
                     range_gate_filter = DBZH < 20 & RHOHV < .95,
                     point_geom=ggplot2::geom_point,
                     point_geom_args=list(),
                     annotate="{round(ff,1)} m/s, {round(dd)}\u00B0",
                     annotation_size=4,
                     vp_color="red"){
  assertthat::assert_that(is.pvol(x))
  if (!is.null(vp)) {
    vp_df <- as.data.frame(vp) |> dplyr::mutate(
      height_bin = glue::glue("{height}-{height + vp$attributes$where$interval} [m]"),
      height_bin = factor(.data$height_bin, levels = .data$height_bin)
    )

    assertthat::assert_that(
      is.null(range),
      msg = "When specifying a vp the range is taken from there. Thus no range should be provided"
    )
    range <- c(vp$attributes$how$minrange, vp$attributes$how$maxrange) * 1000
    if (length(height) == 1) {
      height <- max(vp$data$height[vp$data$height <= height]) + c(0, vp$attributes$where$interval)
    }

    if (!is.null(height)) {
      # if a height profile has been provided we only plot those curves from a vp
      vp_df <- vp_df[(vp_df$height + vp$attributes$where$interval) > min(height) & vp_df$height < max(height), ]
    }
  }
  assertthat::assert_that(
    is.null(range) ||
      (is.numeric(range) && length(2))
  )
  assertthat::assert_that(
    is.null(height) ||
      (is.numeric(height) && length(2)) ||
      (!is.null(vp) && is.numeric(height))
  )
  if (is.null(range)) {
    range <- c(-Inf, Inf)
  }
  if (is.null(height)) {
    height <- c(-Inf, Inf)
  }
  data <-
    mapply(SIMPLIFY = F,
      cbind,
      lapply( lapply(x$scans, scan_to_spatial),
              as.data.frame
      ),
      split(attribute_table(x) |>
              dplyr::mutate(scan_nr=1:dplyr::n())|>
        dplyr::select(-"param"), 1:length(x$scans)), MoreArgs = list(row.names = NULL)
    ) |>
    dplyr::bind_rows() |>
    dplyr::filter(
      !is.na(.data$azim), !is.na(VRADH),
      .data$range > min(!!range), .data$range < max(!!range),
      .data$height < max(!!height), .data$height > min(!!height),
      !!rlang::enexpr(range_gate_filter)
    )
  if (!is.null(vp)) {
    s <- !(is.na(vp_df$ff) | is.na(vp_df$dd))
    # vp_geom <- purrr::pmap(
    #   list(vp_df$ff[s], vp_df$dd[s], vp_df$height[s], vp_df$height_bin[s]),
    #   ~ ggplot2::geom_function(
    #     data = dplyr::bind_cols(data, data.frame(
    #       min_bin_height = ..3,
    #       max_bin_height = ..3 + vp$attributes$where$interval,
    #       height_bin = ..4
    #     )),
    #     fun = function(x, v, a) cos((x - a) / 180 * pi) * v,
    #     args = list(a = ..2, v = ..1),
    #     color = vp_color
    #   )
    # )
    vp_geom <- mapply(SIMPLIFY = F,
      function(spd, dir,hgt, bin)
      {ggplot2::geom_function(
        data = dplyr::bind_cols(data, data.frame(
          min_bin_height = hgt,
          max_bin_height = hgt + vp$attributes$where$interval,
          height_bin = bin
        )),
        fun = function(x, v, a) cos((x - a) / 180 * pi) * v,
        args = list(a = dir, v = spd),
        color = vp_color
      )},

      vp_df$ff[s], vp_df$dd[s], vp_df$height[s], vp_df$height_bin[s]
    )
    if(length(vp_geom)>1){
      vp_geom<-c(vp_geom, ggplot2::facet_wrap(~.data$height_bin))
    }
    int <- findInterval(data$height, c(vp_df$height, max(vp_df$height) + vp$attributes$where$interval))
    int[int == 0] <- NA
    data$height_bin <- vp_df$height_bin[int]
  } else {
    vp_geom <- list()
  }
  if(!is.null(annotate) & is.vp(vp)){
    df<-data.frame(label=as.character(glue::glue_data(annotate, .x=vp_df)),x=Inf,y=Inf, height_bin=vp_df$height_bin)
    annotate_geom<-
     list( ggplot2::geom_label(
        data=df,
        ggplot2::aes(x=x,y=y, label = .data$label),
        vjust = "inward", hjust = "inward", color=vp_color,
        fill=NA, label.size = 0, size=annotation_size
    ))

  }else{
    annotate_geom<-list()
  }
  plt <- ggplot2::ggplot(data, ggplot2::aes(x = .data$azim, y = VRADH)) +
    do.call(point_geom, point_geom_args) +
    ggplot2::scale_x_continuous(breaks = (0:4) * 90, minor_breaks = (0:12) * 30) +
    ggplot2::ylab("Radial velocity [m/s]") +
    ggplot2::xlab("Azimuth [\u00B0]")+
    vp_geom+annotate_geom

  return(plt)
}
