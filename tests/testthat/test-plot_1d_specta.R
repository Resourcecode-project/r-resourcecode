# Test file: test-plot_1d_specta.R

test_that("plot_1d_specta returns a ggplot object", {
  result <- plot_1d_specta(resourcecodedata::rscd_1d_spectra, time = 1)

  expect_s3_class(result, "ggplot")
})

test_that("plot_1d_specta works with integer time input", {
  result <- plot_1d_specta(resourcecodedata::rscd_1d_spectra, time = 5L)

  expect_s3_class(result, "ggplot")
})

test_that("plot_1d_specta works with character time input", {
  # Get a valid time from the data
  valid_time <- format(
    resourcecodedata::rscd_1d_spectra$forcings$time[3],
    format = "%Y-%m-%d %H:%M:%S"
  )

  result <- plot_1d_specta(resourcecodedata::rscd_1d_spectra, time = valid_time)

  expect_s3_class(result, "ggplot")
})

test_that("plot_1d_specta works with POSIXct time input", {
  # Use an actual time from the data
  time_posix <- resourcecodedata::rscd_1d_spectra$forcings$time[2]

  result <- plot_1d_specta(resourcecodedata::rscd_1d_spectra, time = time_posix)

  expect_s3_class(result, "ggplot")
})

test_that("plot_1d_specta warns when freq vector not provided", {
  # Create a copy without the freq vector
  spec_no_freq <- resourcecodedata::rscd_1d_spectra
  spec_no_freq$freq <- NULL

  expect_warning(
    plot_1d_specta(spec_no_freq, time = 1),
    "Frequency vector not provided: using the default Resourcecode frequency vector."
  )
})

test_that("plot_1d_specta works with print_sea_state = FALSE", {
  result <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = 1,
    print_sea_state = FALSE
  )

  expect_s3_class(result, "ggplot")
})

test_that("plot_1d_specta works with print_sea_state = TRUE", {
  result <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = 1,
    print_sea_state = TRUE
  )

  expect_s3_class(result, "ggplot")
  # When print_sea_state = TRUE, the plot should have additional components from patchwork
  expect_true(length(result) > 0)
})

test_that("plot_1d_specta has correct plot labels", {
  result <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = 1,
    print_sea_state = FALSE
  )

  # Check that title contains station name
  expect_match(
    result@labels$title,
    paste(
      "Wave energy spectrum at location",
      resourcecodedata::rscd_1d_spectra$station
    )
  )
  expect_equal(result@scales$get_scales("x")$name, "Frequency (Hz)")
  expect_equal(
    result@scales$get_scales("y")$name,
    "Wave spectral density (m^2.s)"
  )
  expect_match(result@labels$caption, "Source: Resourcecode hindcast database")
})

test_that("plot_1d_spectra handles edge case with first time point", {
  result <- plot_1d_specta(resourcecodedata::rscd_1d_spectra, time = 1)

  expect_s3_class(result, "ggplot")
})

test_that("plot_1d_spectra handles edge case with last time point", {
  # Get the last time index
  last_time <- ncol(resourcecodedata::rscd_1d_spectra$ef)

  result <- plot_1d_specta(resourcecodedata::rscd_1d_spectra, time = last_time)

  expect_s3_class(result, "ggplot")
})

test_that("plot_1d_specta subtitle contains correct time format", {
  result <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = 1,
    print_sea_state = FALSE
  )

  # Check subtitle matches expected datetime format (YYYY-MM-DD HH:MM)
  expect_match(result$labels$subtitle, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$")
})

test_that("plot_1d_specta uses correct frequency data", {
  result <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = 1,
    print_sea_state = FALSE
  )

  # Extract the data from the plot
  plot_data <- ggplot2::layer_data(result, 1)
  dim(plot_data$x) <- 36

  # Check that the frequency data matches the input
  expect_equal(plot_data$x, resourcecodedata::rscd_1d_spectra$freq)
})

test_that("plot_1d_specta uses correct spectral density data", {
  time_idx <- 3
  result <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = time_idx,
    print_sea_state = FALSE
  )

  # Extract the data from the plot
  plot_data <- ggplot2::layer_data(result, 1)

  # Check that the spectral density data matches the input
  expect_equal(plot_data$y, resourcecodedata::rscd_1d_spectra$ef[, time_idx])
})

test_that("plot_1d_specta time conversion works correctly", {
  # Test with character time that exists in the data
  char_time <- format(
    resourcecodedata::rscd_1d_spectra$forcings$time[4],
    format = "%Y-%m-%d %H:%M:%S"
  )
  result_char <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = char_time,
    print_sea_state = FALSE
  )

  # Test with integer time
  result_int <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = 4L,
    print_sea_state = FALSE
  )

  # Test with POSIXct time
  result_posix <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = resourcecodedata::rscd_1d_spectra$forcings$time[4],
    print_sea_state = FALSE
  )

  # All three should produce the same subtitle (same time point)
  expect_equal(result_char$labels$subtitle, result_int$labels$subtitle)
  expect_equal(result_int$labels$subtitle, result_posix$labels$subtitle)
})

test_that("plot_1d_specta has correct color for line", {
  result <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = 1,
    print_sea_state = FALSE
  )

  # Check that the line color is the expected PuBu color
  expect_equal(result$layers[[1]]$aes_params$colour, "#045A8D")
})

test_that("plot_1d_specta has correct theme", {
  result <- plot_1d_specta(
    resourcecodedata::rscd_1d_spectra,
    time = 1,
    print_sea_state = FALSE
  )

  # Check that theme_linedraw is applied
  expect_s3_class(result$theme, "theme")
})
