#' Plot a Velocity Azimuth Display (VAD).
#'
#' A Velocity Azimuth Display visualizes the radial velocity (typically `VRADH`) as a function of the beam azimuth.
#' These plots are useful to assess the quality of radial velocity data (including velocity folding),
#' and for visually inspecting the quality of the velocity fit of a vertical profile estimate.
#'
#' Poor velocity fits in vertical profiles can arise in cases where the movement is not well described by a unidirectional
#' velocity model, or for data with strong velocity folding (i.e. a low Nyquist velocity).
#' @param x An object of class `pvol` or `scan`
#' @param vp An object of class `pvol`, typically a vertical profile estimated for the input polar volume specified under `x` using `bioRad::calculate_vp()`.
#' @param ... Currently not used.
#' @param range_min,range_max Numeric. The minimum and maximum range to include, in m. Values are taken from the `vp`
#' object if provided.
#' @param alt_min,alt_max Numeric. The minimum and maximum altitude to include, in m. If only `alt_min` value is provided
#' next to a `vp` then the height bin intersection with this altitude is plotted.
#' @param range_gate_filter Optional filtering of the range gates. By default range gates are filtered for linear reflectivity
#' value (`eta`) less then 36000 cm^2/km^3 (the default in the profiling algorithm, see `etaMax` in `vol2bird::vol2bird_config()`)
#'  and a correlation coefficient (`RHOHV`) < 0.95. The function selects the first reflectivity factor quantity
#'   from `DBZ`, `DBZH`, `DBZV`, `TH` or `TV` that is present. Alternative filters could be used to highlight specific effects.
#' @param plotting_geom The geom function to visualize the range gates, the default is [ggplot2::geom_point()]. In cases
#' with many data points plots may become cluttered, in which cases alternatives like [ggplot2::geom_bin2d()] or
#'  [ggpointdensity::geom_pointdensity()] may be preferred.
#' @param plotting_geom_args A list with additional arguments to the `plotting_geom` function. For example, controlling the point size or transparancy alpha.
#' @param annotate A [glue][glue::glue()] formating string that is used to annotate the plot with additional properties of the height bin from the `vp`.
#' The string is evaluated using the columns from `as.data.frame(vp)`, any of these columns can thus be used (e.g. `ff` or `sd_vvp`).
#' Use `NULL` if no annotation is desired.
#' @param annotation_color The color used for the `vp` annotations and line.
#' @param annotation_size The text size used for the annotation.
#' @param cosine_correction A character option to select what approach should be taken to correct for the fact
#' that the horizontal velocities are measured at an angle (the elevation angle).
#' For plotting a single scan with one elevation angle the visualized sine curve is
#' adjusted by default for multiple scans multiple scans in a polar volume no correction is aplied unless explicitly selected.
#' See details for more information on the specific options.
#'
#' @returns A [ggplot2::ggplot] object.
#'
#' @details The returned [ggplot2::ggplot] can be styled with additional elements available in the `ggplot2` package.
#' For example, labels and titles can be modified using [ggplot2::labs()] or [ggplot2::ggtitle()].
#' Using [ggplot2::theme()] the visual appearance can easily be modified. Use the regular [+][ggplot2::+.gg]
#' syntax to make modifications (e.g. see the examples for changing the plotting color gradients).
#'
#' As for the radial velocity to plot the first of `VRAD`, `VRADH` or `VRADV` is used.
#'
#' For these plots no vertical velocity is assumed as aeroecology will predominantly move in the horizontal plane.
#' However as the velocity is measured under an angle (the elevation angle of the radar) the radial velocities are an under estimation of the horizontal velocities.
#' To correct this either visualized sine curve from the horizontal velocities can be adjusted (`cosine_correction="vp"`),
#' however if multiple elevation angles are included no single correction can be applied and a warning is raised.
#' Alternatively the measured radial velocity can be corrected (`cosine_correction=="range_gates"`) the advantage is this can be done on multiple elevation scans in the same plot.
#' But with data that has been nyquist folded this correction can introduce extra errors. Therefore with low nyquist velocities a warning is raised.
#'
#' @export
#' @examples
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' example_pvol <- read_pvolfile(pvolfile)
#' # VAD plot for a scan
#' vad(example_scan)
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
#'   plotting_geom_args = list(ggplot2::aes(color = DBZH))
#' ) + viridis::scale_color_virids()
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
                       dplyr::if_all(
                         utils::head(dplyr::matches(c("^DBZ$", "^DBZH$", "^DBZV$", "^TH$", "^TV$")), 1),
                         \(dbz) dbz_to_eta(dbz, !!x$attributes$how$wavelength) < 36000
                       ) &
                         dplyr::if_any(dplyr::matches("RHOHV"), \(rhohv) rhohv < .95),
                     plotting_geom = ggplot2::geom_point,
                     plotting_geom_args = list(),
                     annotate = "{round(ff,1)} m/s, {round(dd)}\u00B0",
                     annotation_size = 4,
                     annotation_color = "red",
                     cosine_correction = c("none", "vp", "range_gates")) {
  assertthat::assert_that(is.pvol(x))
  cosine_correction <- rlang::arg_match(cosine_correction)
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
    is.numeric(alt_min),
    is.numeric(alt_max)
  )

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

  vrad_quantity <- c("VRAD", "VRADH", "VRADV")
  vrad_quantity <- vrad_quantity[vrad_quantity %in% names(data)][1]
  # Filter the plotting data with the height and range, furthermore we omit NA's
  # and apply the range_gate_filter's
  data <- dplyr::filter(
    data,
    !is.na(.data$azim), !is.na(!!rlang::sym(vrad_quantity)),
    .data$range > range_min, .data$range < range_max,
    .data$height < alt_max, .data$height > alt_min,
    !!rlang::enexpr(range_gate_filter)
  )

  # Generate a geom that contains the sine function from the vp
  vp_geom <- list()
  if (!is.null(vp)) {
    # restrict the vp annotation to only those elevation height bins for which we have data
    vp_df <- dplyr::filter(vp_df, !(height + vp$attributes$where$interval < min(data$height) | height > max(data$height)))

    elangles <- unique(data$where.elangle)
    if (length(elangles) != 1 && cosine_correction == "vp") {
      warning("The data to plot is based on multiple elevation angles. To correct the `vp` data one elevation angle is needed, it is therefore averaged resulting in a inperfect correction.")
    }
    mean_elangle_cos <- cospi(mean(elangles) / 180) # TODO should this be a weighted mean?

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
      spd = vp_df$ff[s] * dplyr::if_else(cosine_correction == "vp", mean_elangle_cos, 1),
      dir = vp_df$dd[s],
      hgt = vp_df$height[s],
      bin = vp_df$height_bin[s]
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
  ylab_label <- "Radial velocity [m/s]"
  if (cosine_correction == "range_gates") {
    data[, vrad_quantity] <- data[, vrad_quantity] * (1 / cospi(data$where.elangle / 180))
    ylab_label <- "Corrected radial velocity [m/s]"
    if (min(data$how.NI) < 25) {
      warning("There are data that have a relatively low nyquist velocity (below 25 m/s) meaning nyquist folding is likely to occur. Applying a cosine correction to range gates that are nyquist folded can result in larger deviations then before.")
    }
  }
  if (cosine_correction == "none" && max(data$where.elangle) > acos(.95) / pi * 180) {
    warning("Data with relatively large elevation angles are included. This means larger deviation (above 5%) between the horizontal velocity and radial velocity measured occur. Therefore it is important to consider cosine corrections of the radial velocity")
  }
  # Combine everything in one plot
  plt <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = !!rlang::sym("azim"),
      y = !!rlang::sym(vrad_quantity)
    )
  ) +
    do.call(plotting_geom, plotting_geom_args) +
    ggplot2::scale_x_continuous(breaks = (0:4) * 90, minor_breaks = (0:12) * 30) +
    ggplot2::ylab(ylab_label) +
    ggplot2::xlab("Azimuth [\u00B0]") +
    vp_geom +
    annotate_geom
  return(plt)
}
#' @rdname vad
#' @export
vad.scan <- function(x, vp = NULL, ..., cosine_correction = c("vp", "none", "range_gates")) {
  # construct a pvol to let `vad.pvol` do the heavy lifting however rely on the "vp" cosine correction at it is good for one elevation angle
  pv <- structure(list(scans = list(x), attributes = list(how = list(wavelength = x$attributes$how$wavelength))), class = "pvol")
  vad(pv, vp = vp, ..., cosine_correction = cosine_correction)
}
