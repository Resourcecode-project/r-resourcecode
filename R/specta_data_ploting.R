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
#' @examples
#' plot_2d_specta(resourcecodedata::rscd_2d_spectra, 1)
#' @importFrom ggplot2 ggplot geom_rect scale_x_continuous scale_y_continuous
#'                     theme_linedraw coord_polar scale_color_distiller scale_fill_distiller
#'                     labs expansion
plot_2d_specta <- function(
  spec,
  time = 1L,
  normalize = TRUE,
  trim = 0.01,
  cut_off = 0.4,
  ...
) {
  if (is.character(time)) {
    time <- as.POSIXct(time, tz = "UTC")
  }

  if (inherits(time, "POSIXct")) {
    time <- which(time == spec$forcings$time)
  }

  df_freq <- tidyr::expand_grid(frequency1 = 1:36, dir = spec$dir)

  df_freq$frequency2 <- spec$frequency2[df_freq$frequency1]
  df_freq$frequency1 <- spec$frequency1[df_freq$frequency1]

  df <- tibble::tibble(df_freq, ef = c(spec$efth[, , time]))

  legend_text <- "Power spectrum\n(m^2.s)"

  if (normalize) {
    ef_max <- max(df$ef)
    df$ef <- df$ef / ef_max

    legend_text <- "Normalized\nPower spectrum"
  }

  if (!is.null(trim)) {
    # Trim
    df$ef[df$ef <= trim] <- NA
  }

  df$dir <- (df$dir + 180) %% 360

  ggplot(
    df,
    aes(
      xmin = .data$frequency1,
      xmax = .data$frequency2,
      ymin = dir - 5,
      ymax = dir + 5,
      fill = .data$ef,
      col = .data$ef
    )
  ) +
    geom_rect() +
    scale_x_continuous(
      name = "Frequency (Hz)",
      expand = expansion(),
      limits = c(0, cut_off)
    ) +
    scale_y_continuous(
      name = "Direction from (\u00b0)",
      expand = expansion(),
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
      title = paste(
        "Directional Wave energy spectrum at location",
        spec$station
      ),
      subtitle = format(spec$forcings$time[time], format = "%Y-%m-%d %H:%M"),
      caption = "Source: Resourcecode hindcast database\nresourcecode.ifremer.fr"
    ) +
    theme_linedraw() +
    theme(legend.position = "bottom")
}

#' Plot a wave density 1D spectrum at a given time
#'
#' @param spec the spectral data, as an output from `get_2Dspectrum`
#' @param time the time to plot. Either an integer or the date.
#' @param print_sea_state should the sea_states parameters beeing plot ? Default to TRUE.
#' @param ... currently unused
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot_1d_specta(resourcecodedata::rscd_1d_spectra, 1)
#' @importFrom ggplot2 ggplot geom_line scale_x_continuous scale_y_continuous
#'                     theme_linedraw
#'                     labs expansion
plot_1d_specta <- function(
  spec,
  time = 1L,
  print_sea_state = TRUE,
  ...
) {
  if (is.character(time)) {
    time <- as.POSIXct(time, tz = "UTC")
  }

  if (inherits(time, "POSIXct")) {
    time <- which(time == spec$forcings$time)
  }

  if ("freq" %nin% names(spec)) {
    warning("Frequency vector not provided: using the default Resourcecode frequency vector.")
    freq_plot <- rscd_freq
  } else {
    freq_plot <- spec$freq
  }

  df_plot <- tibble::tibble(
    freq = freq_plot,
    spectral_density = spec$ef[, time]
  )

  out_plot <- ggplot(df_plot,
                     aes(x = .data$freq, y = .data$spectral_density)) +
    geom_line(col = "#045A8D") + # Colour is the darkest from 'PuBu' for consistency with 2d plot
    theme_linedraw() +
    scale_x_continuous(
                       name = "Frequency (Hz)",
                       breaks = seq(from = 0, to = 1, by = 0.2),
                       expand = FALSE) +
    scale_y_continuous(
                       name = latex2exp::TeX("Wave spectral density ($m^2s)"),
                       expand = expansion(c(0, .01), c(0, .05))) +
    labs(
         title = paste("Wave energy spectrum at location", spec$station),
         subtitle = format(spec$forcings$time[time], format = "%Y-%m-%d %H:%M"),
         caption = "Source: Resourcecode hindcast database\nresourcecode.ifremer.fr")
  if (print_sea_state) {
    text <- paste0("**Sea-state parameters:**\n
Hs=", round(spec$forcings$hs[time], 2), "m<br>
Tp=", round(1 / spec$forcings$fp[time], 2), "s<br>
Dir=", round(spec$forcings$dir[time], 2), "\u00b0<br>
Wspd=", round(spec$forcings$wnd[time], 2), "m/s<br>
Wdir=", round(spec$forcings$wnddir[time], 2), "\u00b0<br>")

    out_plot <- out_plot +
      patchwork::inset_element(
                               gridtext::textbox_grob(
                                 text,
                                 box_gp = grid::gpar(col = "black"),
                                 r = grid::unit(5, "pt"),
                                 padding = grid::unit(c(10, 0, 0, 5), "pt"),
                                 margin = grid::unit(c(0, 0, 0, 0), "pt")
                               ),
                               left = 0.7,
                               bottom = 0.77,
                               right = .95,
                               top = .99)
  }
  out_plot
}
