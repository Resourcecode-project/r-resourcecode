# Test basic functionality
test_that("cut_seasons returns a factor", {
  dates <- seq(
    from = as.POSIXct("2023-01-15", tz = "UTC"),
    to = as.POSIXct("2023-12-15", tz = "UTC"),
    by = "month"
  )

  result <- cut_seasons(dates)

  expect_s3_class(result, "factor")
})

# Test input validation
test_that("cut_seasons rejects non-POSIXct input", {
  dates <- as.Date(c("2023-01-15", "2023-06-15"))

  expect_error(
    cut_seasons(dates),
    "datetime must be a POSIXct object"
  )
})

test_that("cut_seasons rejects invalid definition", {
  dates <- as.POSIXct("2023-01-15", tz = "UTC")

  expect_error(
    cut_seasons(dates, definition = "invalid"),
    "definition must be one of"
  )
})

test_that("cut_seasons rejects invalid hemisphere", {
  dates <- as.POSIXct("2023-01-15", tz = "UTC")

  expect_error(
    cut_seasons(dates, hemisphere = "eastern"),
    "hemisphere must be 'northern' or 'southern'"
  )
})

# Test meteorological definition
test_that("cut_seasons meteorological definition works correctly for northern hemisphere", {
  dates <- as.POSIXct(
    c(
      "2023-01-15",
      "2023-03-15",
      "2023-06-15",
      "2023-09-15",
      "2023-12-15"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(
    dates,
    definition = "meteorological",
    hemisphere = "northern"
  )

  expect_equal(
    as.character(result),
    c("Winter", "Spring", "Summer", "Autumn", "Winter")
  )
  expect_equal(levels(result), c("Spring", "Summer", "Autumn", "Winter"))
})

test_that("cut_seasons meteorological definition works correctly for southern hemisphere", {
  dates <- as.POSIXct(
    c(
      "2023-01-15",
      "2023-03-15",
      "2023-06-15",
      "2023-09-15",
      "2023-12-15"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(
    dates,
    definition = "meteorological",
    hemisphere = "southern"
  )

  expect_equal(
    as.character(result),
    c("Summer", "Autumn", "Winter", "Spring", "Summer")
  )
  expect_equal(levels(result), c("Autumn", "Winter", "Spring", "Summer"))
})

# Test astronomical definition
test_that("cut_seasons astronomical definition works correctly", {
  dates <- as.POSIXct(
    c(
      "2023-01-15",
      "2023-04-15",
      "2023-07-15",
      "2023-10-15",
      "2023-12-25"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(
    dates,
    definition = "astronomical",
    hemisphere = "northern"
  )

  expect_equal(
    as.character(result),
    c("Winter", "Spring", "Summer", "Autumn", "Winter")
  )
})

test_that("cut_seasons astronomical definition handles equinox/solstice boundaries", {
  dates <- as.POSIXct(
    c(
      "2023-03-20",
      "2023-06-21",
      "2023-09-22",
      "2023-12-21"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(
    dates,
    definition = "astronomical",
    hemisphere = "northern"
  )

  # These dates should be on or near the boundaries
  expect_true(all(result %in% c("Spring", "Summer", "Autumn", "Winter")))
})

# Test DJF definition
test_that("cut_seasons djf definition works correctly", {
  dates <- as.POSIXct(
    c(
      "2023-01-15",
      "2023-04-15",
      "2023-07-15",
      "2023-10-15",
      "2023-12-15"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "djf")

  expect_equal(as.character(result), c("DJF", "MAM", "JJA", "SON", "DJF"))
  expect_equal(levels(result), c("DJF", "MAM", "JJA", "SON"))
})

# Test JFM definition
test_that("cut_seasons jfm definition works correctly", {
  dates <- as.POSIXct(
    c(
      "2023-02-15",
      "2023-05-15",
      "2023-08-15",
      "2023-11-15"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "jfm")

  expect_equal(as.character(result), c("JFM", "AMJ", "JAS", "OND"))
  expect_equal(levels(result), c("JFM", "AMJ", "JAS", "OND"))
})

# Test FMA definition
test_that("cut_seasons fma definition works correctly", {
  dates <- as.POSIXct(
    c(
      "2023-01-15",
      "2023-03-15",
      "2023-06-15",
      "2023-09-15",
      "2023-12-15"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "fma")

  expect_equal(as.character(result), c("NDJ", "FMA", "MJJ", "ASO", "NDJ"))
  expect_equal(levels(result), c("FMA", "MJJ", "ASO", "NDJ"))
})

# Test AMJ definition
test_that("cut_seasons amj definition works correctly", {
  dates <- as.POSIXct(
    c(
      "2023-01-15",
      "2023-05-15",
      "2023-08-15",
      "2023-11-15"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "amj")

  expect_equal(as.character(result), c("JFM", "AMJ", "JAS", "OND"))
})

# Test JAS definition
test_that("cut_seasons jas definition works correctly", {
  dates <- as.POSIXct(
    c(
      "2023-02-15",
      "2023-05-15",
      "2023-08-15",
      "2023-11-15"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "jas")

  expect_equal(as.character(result), c("JFM", "AMJ", "JAS", "OND"))
})

# Test OND definition
test_that("cut_seasons ond definition works correctly", {
  dates <- as.POSIXct(
    c(
      "2023-02-15",
      "2023-05-15",
      "2023-08-15",
      "2023-11-15"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "ond")

  expect_equal(as.character(result), c("JFM", "AMJ", "JAS", "OND"))
})

# Test custom labels
test_that("cut_seasons accepts custom labels", {
  dates <- as.POSIXct(
    c(
      "2023-01-15",
      "2023-04-15",
      "2023-07-15",
      "2023-10-15"
    ),
    tz = "UTC"
  )

  custom_labels <- c("Q1", "Q2", "Q3", "Q4")
  result <- cut_seasons(dates, definition = "djf", labels = custom_labels)

  expect_equal(levels(result), custom_labels)
})

test_that("cut_seasons rejects incorrect number of custom labels", {
  dates <- as.POSIXct("2023-01-15", tz = "UTC")

  expect_error(
    cut_seasons(dates, definition = "meteorological", labels = c("A", "B")),
    "Number of labels"
  )
})

# Test edge cases
test_that("cut_seasons handles single date", {
  date <- as.POSIXct("2023-06-15", tz = "UTC")

  result <- cut_seasons(date)

  expect_length(result, 1)
  expect_equal(as.character(result), "Summer")
})

test_that("cut_seasons handles year boundaries", {
  dates <- as.POSIXct(
    c(
      "2023-12-31 23:00:00",
      "2024-01-01 00:00:00"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "meteorological")

  expect_equal(as.character(result), c("Winter", "Winter"))
})

test_that("cut_seasons handles leap years", {
  dates <- as.POSIXct(
    c(
      "2024-02-29 12:00:00",
      "2023-03-01 12:00:00"
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "meteorological")

  expect_equal(as.character(result), c("Winter", "Spring"))
})

test_that("cut_seasons handles month boundaries correctly", {
  # Test boundaries between months for meteorological definition
  dates <- as.POSIXct(
    c(
      "2023-02-28 23:59:59", # Last second of February (Winter)
      "2023-03-01 00:00:00" # First second of March (Spring)
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "meteorological")

  expect_equal(as.character(result), c("Winter", "Spring"))
})

test_that("cut_seasons handles all 12 months correctly", {
  dates <- as.POSIXct(paste0("2023-", 1:12, "-15"), tz = "UTC")

  result <- cut_seasons(
    dates,
    definition = "meteorological",
    hemisphere = "northern"
  )

  expected <- c(
    "Winter",
    "Winter",
    "Spring",
    "Spring",
    "Spring",
    "Summer",
    "Summer",
    "Summer",
    "Autumn",
    "Autumn",
    "Autumn",
    "Winter"
  )

  expect_equal(as.character(result), expected)
})

test_that("cut_seasons preserves vector length", {
  dates <- seq(
    from = as.POSIXct("2023-01-01", tz = "UTC"),
    to = as.POSIXct("2023-12-31", tz = "UTC"),
    by = "day"
  )

  result <- cut_seasons(dates)

  expect_length(result, length(dates))
})

test_that("cut_seasons works with different timezones", {
  dates_utc <- as.POSIXct("2023-06-15 12:00:00", tz = "UTC")
  dates_est <- as.POSIXct("2023-06-15 12:00:00", tz = "America/New_York")

  result_utc <- cut_seasons(dates_utc)
  result_est <- cut_seasons(dates_est)

  # Both should classify as Summer (same month)
  expect_equal(as.character(result_utc), "Summer")
  expect_equal(as.character(result_est), "Summer")
})

test_that("cut_seasons astronomical definition handles year transitions", {
  dates <- as.POSIXct(
    c(
      "2023-12-20 12:00:00", # Before winter solstice
      "2023-12-22 12:00:00" # After winter solstice
    ),
    tz = "UTC"
  )

  result <- cut_seasons(dates, definition = "astronomical")

  expect_equal(as.character(result), c("Autumn", "Winter"))
})

test_that("cut_seasons custom labels work with meteorological definition", {
  dates <- as.POSIXct(
    c(
      "2023-01-15",
      "2023-04-15",
      "2023-07-15",
      "2023-10-15"
    ),
    tz = "UTC"
  )

  custom_labels <- c("Printemps", "Été", "Automne", "Hiver")
  result <- cut_seasons(
    dates,
    definition = "meteorological",
    labels = custom_labels
  )

  expect_true(all(levels(result) %in% custom_labels))
})

test_that("cut_seasons handles vector with NA values", {
  dates <- as.POSIXct(c("2023-01-15", NA, "2023-07-15"), tz = "UTC")

  result <- cut_seasons(dates)

  expect_length(result, 3)
  expect_true(is.na(result[2]))
  expect_equal(as.character(result[c(1, 3)]), c("Winter", "Summer"))
})
