#' Plot a wave density 2D spectrum
#'
#' @param spec the spectral data, as an output from `get_2Dspectrum`
#' @param time the time to plot. Either an integer or the date.
#' @param normalize Should the spectrum be normalized to have maimum 1 before ploting
#' @param trim removes the values of the spectral density lower than this value
#' @param cut_off cut-off frequency above which the spectrum is not plotted
#' @param ... currently unused
#'
#' @return a ggplot object
#' @export
#'
#' @examplesIf curl::has_internet()
#' spec <- get_2Dspectrum("SEMREVO", start = "1994-01-01", end = "1994-01-31")
#' plot_2Dspecta(spec, 1)
#' @importFrom ggplot2 ggplot geom_rect scale_x_continuous scale_y_continuous
#'                     theme_bw coord_polar scale_color_distiller scale_fill_distiller
#'                     labs expansion
plot_2Dspecta <- function(spec, time = 1L, normalize = TRUE, trim = 0.01, cut_off = .4, ...) {
  if (is.character(time)) {
    time <- as.POSIXct(time, tz = "UTC")
  }

  if ("POSIXct" %in% class(time)) {
    time <- which(time == spec$forcings$time)
  }

  df_freq <- tidyr::expand_grid(frequency1 = 1:36, dir = spec$dir)

  df_freq$frequency2 <- spec$frequency2[df_freq$frequency1]
  df_freq$frequency1 <- spec$frequency1[df_freq$frequency1]

  df <- tibble::tibble(df_freq, ef = c(spec$efth[, , time]))

  legend_text <- "Power spectrum\n(m^2.s)"

  if (normalize) {
    ef.max <- max(df$ef)
    df$ef <- df$ef / ef.max

    legend_text <- "Normalized\nPower spectrum"
  }

  if (!is.null(trim)) {
    # Trim
    df$ef[df$ef <= trim] <- NA
  }


  df$dir <- (df$dir + 180) %% 360

  ggplot(df, aes(
    xmin = .data$frequency1, xmax = .data$frequency2,
    ymin = dir - 5, ymax = dir + 5,
    fill = .data$ef, col = .data$ef
  )) +
    geom_rect() +
    scale_x_continuous(name = "Frequency (Hz)", expand = expansion(), limits = c(0, cut_off)) +
    scale_y_continuous(
      name = "Direction from (\u00b0)", expand = expansion(),
      breaks = c(0, 90, 180, 270),
      labels = c("N", "E", "S", "W"),
      minor_breaks = seq(from = 0, to = 360, by = 30)
    ) +
    coord_polar(theta = "y", start = -5 * pi / 180) +
    scale_color_distiller(
      palette = "PuBu",
      direction = 1,
      name = legend_text,
      na.value = "transparent"
    ) +
    scale_fill_distiller(
      palette = "PuBu",
      direction = 1,
      name = legend_text,
      na.value = "transparent"
    ) +
    labs(
      title = paste("Directional Wave energy spectrum at location", spec$station),
      subtitle = format(spec$forcings$time[time], format = "%Y-%m-%d %H:%M"),
      caption = "Source: Resourcecode hindcast database\nresourcecode.ifremer.fr"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
}
