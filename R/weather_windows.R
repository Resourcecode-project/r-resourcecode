#' Compute Weather Windows
#'
#' Computes and returns start date of each weather window, implemented in C++ for speed.
#'
#' @param valid_periods A data frame with a 'time' column (POSIXct).
#' @param window_length Minimum window duration (hours).
#' @param allow_overlap Logical; If TRUE, the algorithm searches for window,
#' if a window is found, search of next window will start from the end of the previous window.
#' If FALSE, it uses continuous window search: The algorithm searches for window starting from
#' every time step that meets the criteria.
#' @param time_step Expected time step between consecutive timestamps (seconds).
#'
#' @return POSIXct vector of detected window start times.
#' @export
weather_windows <- function(valid_periods, window_length, allow_overlap = TRUE, time_step = 3600) {
  if ("time" %nin% names(valid_periods)) {
    stop("'valid_periods' must be a data frame with a 'time' column (POSIXct)")
  }
  times_num <- as.numeric(valid_periods$time)
  detected <- ww_calc_cpp(times_num, window_length, allow_overlap, time_step)
  lubridate::as_datetime(detected)
}
