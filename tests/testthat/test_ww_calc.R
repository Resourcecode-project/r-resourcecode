# Helper: create hourly sequence with optional gaps
make_times <- function(start, end, by = "hour", gaps = NULL) {
  t <- seq.POSIXt(as.POSIXct(start), as.POSIXct(end), by = by)
  if (!is.null(gaps)) t <- t[-gaps]
  data.frame(time = t)
}

test_that("'weather_windows()' detects windows correctly with overlap", {
  df <- make_times("2020-01-01 00:00:00", "2020-01-02 23:00:00")

  # 24-hour windows, allow overlap
  res <- weather_windows(df, window_length = 24, allow_overlap = TRUE, time_step = 3600)

  expect_true(length(res) > 0)
  expect_true(all(res >= df$time[1]))
  expect_true(all(diff(res) >= 1)) # minimal check: diff positive
})

test_that("'weather_windows()' respects allow_overlap = FALSE", {
  df <- make_times("2020-01-01 00:00:00", "2020-01-03 23:00:00")

  res_overlap <- weather_windows(df, window_length = 24, allow_overlap = TRUE)
  res_no_overlap <- weather_windows(df, window_length = 24, allow_overlap = FALSE)

  expect_true(length(res_overlap) <= length(res_no_overlap))
  expect_true(all(diff(res_overlap, units = "hours") >= 24)) # no overlapping windows
})

test_that("'weather_windows()' handles gaps correctly", {
  # Remove some timestamps to simulate gaps
  df <- make_times("2020-01-01 00:00:00", "2020-01-02 23:00:00", gaps = c(10, 11, 12))

  res <- weather_windows(df, window_length = 5, allow_overlap = TRUE)
  # Ensure all returned starts are within the original data
  expect_true(all(res %in% df$time))
})

test_that("'weather_windows()' returns empty for insufficient data", {
  df <- data.frame(time = as.POSIXct("2020-01-01 00:00:00"))
  expect_equal(length(weather_windows(df, window_length = 1)), 0)

  df2 <- make_times("2020-01-01 00:00:00", "2020-01-01 01:00:00")
  # window_length longer than data
  expect_equal(length(weather_windows(df2, window_length = 5)), 0)
})

test_that("'weather_windows()' handles non-hourly time_step", {
  df <- make_times("2020-01-01 00:00:00", "2020-01-01 06:00:00", by = "2 hours")

  res <- weather_windows(df, window_length = 4, allow_overlap = TRUE, time_step = 2 * 3600)
  expect_true(length(res) > 0)
})
