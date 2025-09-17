#' Create a Velocity Azimuth Display (VAD) plot
#'
#' A velocity azimuth display plot visualized the radial velocity (`VRAD`) as a function of the azimuth from the radar.
#' Among others it can be used to asses the fit of the movement speeds in a vertical profile.
#' For example, when the movement is not uniform but rather in multiple directions or rotating this will be visible as
#' deviations from the sine function.
#'
#' @param x A polar volume from which range gates are extracted.
#' @param ... Currently not used.
#' @param vp A vertical profile to annotate the VAD plot with the fit in the vertical profile
#' @param range_min,range_max The distance range in to filter the range gates with.
#'      If a `vp` is provided the range is taken from the `vp` and the argument `range_min` and `range_max` should not be provided.
#' @param alt_min,alt_max The altitude range to filter the range gates by.
#'      If only `alt_min` value is provided next to a `vp` then the height bin intersection with this altitude is plotted.
#' @param range_gate_filter Optional filtering of the range gates. By default range gates are filtered for a eta (reflectivity)
#'      value less then 36000 (the vol2bird default) and a `RHOHV` less then 0.95.
#'      The function selects the first reflectivity factor quantity from `DBZ`, `DBZH`, `DBZV`, `TH` or `TV` that is present.
#'      Alternative filters could be used to highlight specific effects.
#' @param plotting_geom The geom function to visualize the range gates, the default is [ggplot2::geom_point()], in some cases this suffers from over plotting.
#'      Alternatives that avoid over plotting could be [ggplot2::geom_bin2d()] or [ggpointdensity::geom_pointdensity()].
#' @param plotting_geom_args A list with additional arguments to the `plotting_geom` function. For example, controlling the point size or alpha.
#' @param annotate A [glue][glue::glue()] formating string that is used to annotate the plot with additional properties of the height bin from the `vp`.
#'      The string is evaluated using the columns from `as.data.frame(vp)`, any of these columns can thus be used (e.g. `ff` or `sd_vvp`).
#'      Use `NULL` if no annotation is desired.
#' @param annotation_color The color used for the `vp` annotations and line.
#' @param annotation_size The text size used for the annotation.
#'
#' @returns A [ggplot2::ggplot] object.
#'
#' @details  As a [ggplot2::ggplot] object  us returned additional elements can be added using regular function in
#'      the `ggplot2` package. Labels could, for example, be modified using [ggplot2::labs()] or [ggplot2::ggtitle()].
#'      Using [ggplot2::theme()] the visual appearance can easily be modified. To do this the regular [+][ggplot2::+.gg]
#'      syntax can be used. In the examples this is demonstrated using scale.
#'
#'      As for the radial velocity to plot the first of `VRAD`, `VRADH` or `VRADV` is used.
#'
#' @export
#' @examples
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' example_pvol <- read_pvolfile(pvolfile)
#' # VAD plots can be created for polar volumes alone
#' vad(example_pvol,
#'   range_min = 5000, range_max = 30000,
#'   alt_min = 200, alt_max = 400
#' )
#' # It is also possible to plot one height or more height bins from a `vp`.
#' # Many visual aspects can be controlled through the function arguments
#' # or through adding `ggplot`
#' vp <- calculate_vp(pvolfile)
#' vad(example_pvol,
#'   vp = vp,
#'   alt_min = 400,
#'   plotting_geom_args = list(ggplot2::aes(color = ZDR))
#' ) + ggplot2::scale_color_gradient2()
#' vad(example_pvol,
#'   vp = vp, alt_min = 400, alt_max = 1200,
#'   plotting_geom = ggplot2::geom_bin2d,
#'   annotate = "sdvvp: {round(sd_vvp,2)} [m/s]",
#' ) + ggplot2::scale_fill_viridis_c()
vad <- function(x, ...) {
  UseMethod("vad", x)
}
#' @rdname vad
#' @export
vad.pvol <- function(x, vp = NULL, ...,
                     range_min = NULL, range_max = NULL,
                     alt_min = NULL, alt_max = NULL,
                     range_gate_filter =
                       dplyr::if_all(utils::head(dplyr::matches(c("^DBZ$","^DBZH$","^DBZV$","^TH$","^TV$")),1),
                                     \(dbz) dbz_to_eta(dbz, !!x$attributes$how$wavelength)<36000)&
                       dplyr::if_any(dplyr::matches("RHOHV"),  \(rhohv) rhohv <.95),
                     plotting_geom = ggplot2::geom_point,
                     plotting_geom_args = list(),
                     annotate = "{round(ff,1)} m/s, {round(dd)}\u00B0",
                     annotation_size = 4,
                     annotation_color = "red") {
  assertthat::assert_that(is.pvol(x))
  # Some of the input variables are checked and modified specifically in the VP context
  if (!is.null(vp)) {
    assertthat::assert_that(is.vp(vp))
    # For vp's we take range from the vp and not the arguments
    assertthat::assert_that(
      is.null(range_min) && is.null(range_max),
      msg = "When specifying a vp the range is taken from there. Thus no range should be provided."
    )
    range_min <- vp$attributes$how$minrange * 1000
    range_max <- vp$attributes$how$maxrange * 1000
    # convert the vp to df with height as we will need it later
    vp_df <- as.data.frame(vp) |> dplyr::mutate(
      height_bin = glue::glue("{height}-{height + vp$attributes$where$interval} [m]"),
      height_bin = factor(.data$height_bin, levels = .data$height_bin)
    )
    # For one height we take the interval that intersects
    if (is.numeric(alt_min) && rlang::is_scalar_vector(alt_min) && is.null(alt_max)) {
      alt_min <- max(vp$data$height[vp$data$height <= alt_min])
      alt_max <- alt_min + vp$attributes$where$interval
    }
    # if a height profile has been provided we only plot those curves from a vp and thus subset `vp_df`
    if (!is.null(alt_min)) {
      vp_df <- vp_df[(vp_df$height + vp$attributes$where$interval) > alt_min, ]
    }
    if (!is.null(alt_max)) {
      vp_df <- vp_df[vp_df$height < alt_max, ]
    }
  }
  # Checking of input variables
  assertthat::assert_that(
    is.null(range_min) || rlang::is_scalar_vector(range_min),
    is.null(range_max) || rlang::is_scalar_vector(range_max)
  )
  range_min <- max(-Inf, range_min)
  range_max <- min(Inf, range_max)
  assertthat::assert_that(
    is.numeric(range_min),
    is.numeric(range_max),
    is.null(alt_min) || rlang::is_scalar_vector(alt_min),
    is.null(alt_max) || rlang::is_scalar_vector(alt_max)

    )
  alt_min <- max(-Inf, alt_min)
  alt_max <- min(Inf, alt_max)
  assertthat::assert_that(
    is.numeric(alt_min) ,
    is.numeric(alt_max) )

  # Convert the polar volume to plotting data by converting the scans to locations
  data <-
    mapply(
      SIMPLIFY = F,
      cbind,
      lapply(
        lapply(x$scans, scan_to_spatial),
        as.data.frame
      ),
      # We add extra attributes from the scan attributes that can be useful for filtering or highlighting
      # certain attributes
      split(attribute_table(x) |>
        dplyr::mutate(scan_nr = 1:dplyr::n()) |>
        dplyr::select(-"param"), 1:length(x$scans)), MoreArgs = list(row.names = NULL)
    ) |>
    dplyr::bind_rows()

    vrad_quantity<-c("VRAD","VRADH","VRADV")
  vrad_quantity<-vrad_quantity[vrad_quantity %in%names(data)][1]
    # Filter the plotting data with the height and range, furthermore we omit NA's
    # and apply the range_gate_filter's
  data<-  dplyr::filter(data,
      !is.na(.data$azim), !is.na(!!rlang::sym(vrad_quantity)),
      .data$range > range_min, .data$range < range_max,
      .data$height < alt_max, .data$height > alt_min,
      !!rlang::enexpr(range_gate_filter)
    )
  # Generate a geom that contains the sine function from the vp
  vp_geom <- list()
  if (!is.null(vp)) {
    s <- !(is.na(vp_df$ff) | is.na(vp_df$dd))
    vp_geom <- mapply(
      SIMPLIFY = F,
      function(spd, dir, hgt, bin) {
        ggplot2::geom_function(
          data = dplyr::bind_cols(data, data.frame(
            min_bin_height = hgt,
            max_bin_height = hgt + vp$attributes$where$interval,
            height_bin = bin
          )),
          fun = function(x, v, a) cos((x - a) / 180 * pi) * v,
          args = list(a = dir, v = spd),
          color = annotation_color
        )
      },
      vp_df$ff[s], vp_df$dd[s], vp_df$height[s], vp_df$height_bin[s]
    )
    if (length(vp_geom) > 1) {
      vp_geom <- c(vp_geom, ggplot2::facet_wrap(~ .data$height_bin))
    }
    int <- findInterval(data$height, c(vp_df$height, max(vp_df$height) + vp$attributes$where$interval))
    int[int == 0] <- NA
    data$height_bin <- vp_df$height_bin[int]
  }
  # Create the `annotate_geom` for textual annotations of each height interval
  # The glue string is annotated in the `vp_df` so that all vp attributes are available
  annotate_geom <- list()
  if (!is.null(annotate) & is.vp(vp)) {
    df <- data.frame(
      label = as.character(glue::glue_data(annotate, .x = vp_df)),
      x = Inf, y = Inf, height_bin = vp_df$height_bin
    )
    annotate_geom <-
      list(ggplot2::geom_label(
        data = df,
        ggplot2::aes(x = x, y = y, label = .data$label),
        vjust = "inward", hjust = "inward", color = annotation_color,
        fill = NA, label.size = 0, size = annotation_size
      ))
  }


  # Combine everything in one plot
  plt <- ggplot2::ggplot(data,
                         ggplot2::aes(x = !!rlang::sym("azim"),
                                      y = !!rlang::sym(vrad_quantity))) +
    do.call(plotting_geom, plotting_geom_args) +
    ggplot2::scale_x_continuous(breaks = (0:4) * 90, minor_breaks = (0:12) * 30) +
    ggplot2::ylab("Radial velocity [m/s]") +
    ggplot2::xlab("Azimuth [\u00B0]") +
    vp_geom +
    annotate_geom

  return(plt)
}
