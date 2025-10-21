test_that("fractional_day_of_year returns 0 for January 1st at midnight", {
  dt <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  expect_equal(fractional_day_of_year(dt), 0)

  # Test for different year
  dt2 <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  expect_equal(fractional_day_of_year(dt2), 0)
})

test_that("fractional_day_of_year returns correct value for December 31st at 23:00", {
  # Non-leap year (2023)
  dt_2023 <- as.POSIXct("2023-12-31 23:00:00", tz = "UTC")
  expect_equal(fractional_day_of_year(dt_2023), 364 + 23 / 24, tolerance = 1e-6)

  # Leap year (2024)
  dt_2024 <- as.POSIXct("2024-12-31 23:00:00", tz = "UTC")
  expect_equal(fractional_day_of_year(dt_2024), 365 + 23 / 24, tolerance = 1e-6)
})

test_that("fractional_day_of_year correctly handles hourly increments", {
  dates <- seq(
    from = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    to = as.POSIXct("2024-01-01 23:00:00", tz = "UTC"),
    by = "hour"
  )

  result <- fractional_day_of_year(dates)
  expected <- seq(0, 23 / 24, by = 1 / 24)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_length(result, 24)
})

test_that("fractional_day_of_year correctly handles noon", {
  dt <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  expect_equal(fractional_day_of_year(dt), 0.5)

  # Mid-year
  dt_midyear <- as.POSIXct("2024-07-01 12:00:00", tz = "UTC")
  # July 1st is day 183 in a leap year (182 complete days have passed)
  expect_equal(fractional_day_of_year(dt_midyear), 182.5, tolerance = 1e-6)
})

test_that("fractional_day_of_year handles leap years correctly", {
  # February 29th in leap year
  dt_leap <- as.POSIXct("2024-02-29 00:00:00", tz = "UTC")
  # Feb 29 is the 60th day (59 complete days have passed)
  expect_equal(fractional_day_of_year(dt_leap), 59)

  # March 1st in leap year vs non-leap year
  dt_leap_mar <- as.POSIXct("2024-03-01 00:00:00", tz = "UTC")
  dt_nonleap_mar <- as.POSIXct("2023-03-01 00:00:00", tz = "UTC")

  expect_equal(fractional_day_of_year(dt_leap_mar), 60) # Day 61 in leap year
  expect_equal(fractional_day_of_year(dt_nonleap_mar), 59) # Day 60 in non-leap year
})

test_that("fractional_day_of_year works with vectors", {
  dates <- c(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 06:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 18:00:00", tz = "UTC")
  )

  result <- fractional_day_of_year(dates)
  expected <- c(0, 0.25, 0.5, 0.75)

  expect_equal(result, expected)
  expect_length(result, 4)
})

test_that("fractional_day_of_year handles different timezones", {
  dt_utc <- as.POSIXct("2024-03-07 12:00:00", tz = "UTC")
  dt_est <- as.POSIXct("2024-03-07 12:00:00", tz = "America/New_York")

  # Both should calculate based on their respective midnight Jan 1
  result_utc <- fractional_day_of_year(dt_utc)
  result_est <- fractional_day_of_year(dt_est)

  # They should give the same fractional day for the same local time
  expect_equal(result_utc, result_est)

  # June 15 is day 167 in a leap year (166 complete days)
  expect_equal(result_utc, 66.5, tolerance = 1e-6)
})

test_that("fractional_day_of_year errors with non-POSIXct input", {
  expect_error(
    fractional_day_of_year("2024-01-01"),
    "datetime must be a POSIXct object"
  )

  expect_error(
    fractional_day_of_year(as.Date("2024-01-01")),
    "datetime must be a POSIXct object"
  )

  expect_error(
    fractional_day_of_year(123456),
    "datetime must be a POSIXct object"
  )
})

test_that("fractional_day_of_year handles edge cases across year boundary", {
  # Last hour of year
  dt_last <- as.POSIXct("2024-12-31 23:00:00", tz = "UTC")
  result_last <- fractional_day_of_year(dt_last)

  # First hour of next year
  dt_first <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  result_first <- fractional_day_of_year(dt_first)

  expect_equal(result_first, 0)
  expect_gt(result_last, 365) # Should be greater than 365
})

test_that("fractional_day_of_year is consistent with sequential dates", {
  dates <- seq(
    from = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    to = as.POSIXct("2024-01-10 00:00:00", tz = "UTC"),
    by = "day"
  )

  result <- fractional_day_of_year(dates)

  # Should increase by 1 each day
  diffs <- diff(result)
  expect_true(all(abs(diffs - 1) < 1e-10))

  # First value should be 0, last should be 9
  expect_equal(result[1], 0)
  expect_equal(result[length(result)], 9)
})

test_that("fractional_day_of_year handles NA values", {
  dates <- c(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct(NA, tz = "UTC"),
    as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  )

  result <- fractional_day_of_year(dates)

  expect_equal(result[1], 0)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 0.5)
})
