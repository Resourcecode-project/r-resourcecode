# ==============================================================================
# Test Suite 1: Input Validation
# ==============================================================================

test_that("Input validation works correctly", {

  # Test non-POSIXct input
  expect_error(cut_seasons("2023-01-01"),
               "datetime must be a POSIXct object")

  expect_error(cut_seasons(as.Date("2023-01-01")),
               "datetime must be a POSIXct object")

  expect_error(cut_seasons(c(1, 2, 3)),
               "datetime must be a POSIXct object")

  # Test invalid definition
  expect_error(cut_seasons(as.POSIXct("2023-01-01"), definition = "invalid"),
               "definition must be one of: 'meteorological', 'astronomical',
         'djf', 'jfm', 'amj', 'jas', 'ond', 'fma'")

  # Test invalid hemisphere
  expect_error(cut_seasons(as.POSIXct("2023-01-01"), hemisphere = "eastern"),
               "hemisphere must be 'northern' or 'southern'")

  # Test label length mismatch
  test_date <- as.POSIXct("2023-01-01")
  expect_error(cut_seasons(test_date, definition = "djf", labels = c("Only", "Two")),
               "Number of labels .* must match number of unique seasons")
})

# ==============================================================================
# Test Suite 2: Meteorological Seasons - Northern Hemisphere
# ==============================================================================

test_that("Meteorological seasons work correctly for Northern Hemisphere", {

  # Test representative dates for each season
  winter_date <- as.POSIXct("2023-01-15")
  spring_date <- as.POSIXct("2023-04-15")
  summer_date <- as.POSIXct("2023-07-15")
  autumn_date <- as.POSIXct("2023-10-15")

  test_dates <- c(winter_date, spring_date, summer_date, autumn_date)
  seasons <- cut_seasons(test_dates, "meteorological", "northern")

  expect_equal(as.character(seasons), c("Winter", "Spring", "Summer", "Autumn"))

  # Test boundary dates
  feb_end <- as.POSIXct("2023-02-28")
  mar_start <- as.POSIXct("2023-03-01")

  expect_equal(as.character(cut_seasons(feb_end, "meteorological")), "Winter")
  expect_equal(as.character(cut_seasons(mar_start, "meteorological")), "Spring")

  # Test December (winter)
  dec_date <- as.POSIXct("2023-12-15")
  expect_equal(as.character(cut_seasons(dec_date, "meteorological")), "Winter")

  # Test factor levels are in correct order
  expect_equal(levels(seasons), c("Spring", "Summer", "Autumn", "Winter"))
})
