# Tests for get_1d_spectrum()
test_that("get_1d_spectrum retrieves data successfully", {
  #vcr::local_cassette("get_1d_spectrum_basic")

  spec <- get_1d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-02-28"
  )

  expect_type(spec, "list")
  expect_named(
    spec,
    c(
      "longitude",
      "latitude",
      "frequency1",
      "frequency2",
      "ef",
      "th1m",
      "th2m",
      "sth1m",
      "sth2m",
      "freq",
      "forcings",
      "station"
    )
  )
  expect_s3_class(spec$forcings, "data.frame")
  expect_shape(spec$forcings, dim = c(1416, 14))
  expect_equal(spec$station, "SEMREVO")
})

test_that("get_1d_spectrum handles numeric node input", {
  #vcr::local_cassette("get_1d_spectrum_numeric_node")

  spec_by_name <- get_1d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-01-31"
  )

  # Find the index of SEMREVO in rscd_spectral
  idx <- which(resourcecodedata::rscd_spectral$name == "SEMREVO")

  spec_by_index <- get_1d_spectrum(
    idx,
    start = "1994-01-01",
    end = "1994-01-31"
  )

  expect_equal(spec_by_name$station, spec_by_index$station)
})

test_that("get_1d_spectrum handles character date inputs", {
  #vcr::local_cassette("get_1d_spectrum_character_dates")

  spec <- get_1d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-01-31"
  )

  expect_type(spec, "list")
  expect_shape(spec$forcings, dim = c(744, 14))
})

test_that("get_1d_spectrum handles numeric (UNIX timestamp) date inputs", {
  # vcr::local_cassette("get_1d_spectrum_numeric_dates")

  start_unix <- as.numeric(as.POSIXct("1994-01-01", tz = "UTC"))
  end_unix <- as.numeric(as.POSIXct("1994-01-31", tz = "UTC"))

  spec <- get_1d_spectrum(
    "SEMREVO",
    start = start_unix,
    end = end_unix
  )

  expect_type(spec, "list")
  expect_gt(nrow(spec$forcings), 0)
})

test_that("get_1d_spectrum handles multi-month requests", {
  # vcr::local_cassette("get_1d_spectrum_multi_month")

  spec <- get_1d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-03-31"
  )

  expect_type(spec, "list")
  # Should have data for 3 months
  expect_gt(nrow(spec$forcings), 2000) # Rough estimate
})

test_that("get_1d_spectrum validates forcings data structure", {
  # vcr::local_cassette("get_1d_spectrum_forcings_structure")

  spec <- get_1d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-01-31"
  )

  # Check forcings has expected columns
  expected_cols <- c(
    "time",
    "dpt",
    "wnd",
    "wnddir",
    "cur",
    "curdir",
    "hs",
    "fp",
    "f02",
    "f0m1",
    "th1p",
    "sth1p",
    "dir",
    "spr"
  )
  expect_true(all(expected_cols %in% names(spec$forcings)))

  # Check time is POSIXct
  expect_s3_class(spec$forcings$time, "POSIXct")
})

test_that("get_1d_spectrum validates spectral arrays dimensions", {
  # vcr::local_cassette("get_1d_spectrum_array_dimensions")

  spec <- get_1d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-01-31"
  )

  # Check that spectral arrays have correct dimensions
  expect_true(is.matrix(spec$ef))
  expect_equal(nrow(spec$ef), length(spec$freq))
  expect_equal(ncol(spec$ef), nrow(spec$forcings))
})

# Error handling tests - input validation (no network needed)
test_that("get_1d_spectrum validates point input length", {
  expect_error(
    get_1d_spectrum(
      c("SEMREVO", "AUTRE"),
      start = "1994-01-01",
      end = "1994-01-31"
    ),
    "length\\(point\\) == 1"
  )
})

test_that("get_1d_spectrum validates point exists", {
  expect_error(
    get_1d_spectrum(
      "INVALID_POINT",
      start = "1994-01-01",
      end = "1994-01-31"
    ),
    "point %in% resourcecodedata::rscd_spectral\\$name"
  )
})

test_that("get_1d_spectrum validates date range", {
  expect_error(
    get_1d_spectrum(
      "SEMREVO",
      start = "1994-01-31",
      end = "1994-01-01"
    ),
    "end >= start"
  )
})

test_that("get_1d_spectrum validates start date is within coverage", {
  expect_error(
    get_1d_spectrum(
      "SEMREVO",
      start = "1980-01-01", # Before hindcast period
      end = "1994-01-31"
    ),
    "format\\(start, \"%Y\"\\) >= format\\(rscd_hindcast_start_date"
  )
})

test_that("get_1d_spectrum validates end date is within coverage", {
  expect_error(
    get_1d_spectrum(
      "SEMREVO",
      start = "1994-01-01",
      end = "2030-01-01" # After hindcast period
    ),
    "format\\(end, \"%Y\"\\) <= format\\(rscd_hindcast_end_date"
  )
})

# Network failure tests - using mocks (no vcr needed)
test_that("get_1d_spectrum fails gracefully when first download fails", {
  # Mock the internal raw function to return NULL (simulating download failure)
  mockery::stub(
    get_1d_spectrum,
    "get_1d_spectrum_raw",
    NULL
  )

  # Should fail or return NULL depending on your implementation
  # Update this based on how you handle NULL in get_1d_spectrum
  expect_null(
    get_1d_spectrum(
      "SEMREVO",
      start = "1994-01-01",
      end = "1994-01-31"
    )
  )
})

# Tests for get_2d_spectrum()
test_that("get_2d_spectrum retrieves data successfully", {
  # vcr::local_cassette("get_2d_spectrum_basic")

  spec <- get_2d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-02-28"
  )

  expect_type(spec, "list")
  expect_named(
    spec,
    c(
      "longitude",
      "latitude",
      "frequency1",
      "frequency2",
      "efth",
      "freq",
      "dir",
      "forcings",
      "station"
    )
  )
  expect_s3_class(spec$forcings, "data.frame")
  expect_shape(spec$forcings, dim = c(1416, 6))
  expect_equal(spec$station, "SEMREVO")
})

test_that("get_2d_spectrum handles numeric node input", {
  # vcr::local_cassette("get_2d_spectrum_numeric_node")

  spec_by_name <- get_2d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-01-31"
  )

  idx <- which(resourcecodedata::rscd_spectral$name == "SEMREVO")

  spec_by_index <- get_2d_spectrum(
    idx,
    start = "1994-01-01",
    end = "1994-01-31"
  )

  expect_equal(spec_by_name$station, spec_by_index$station)
})

test_that("get_2d_spectrum handles character date inputs", {
  # vcr::local_cassette("get_2d_spectrum_character_dates")

  spec <- get_2d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-01-31"
  )

  expect_type(spec, "list")
  expect_shape(spec$forcings, dim = c(744, 6))
})

test_that("get_2d_spectrum handles numeric date inputs", {
  # vcr::local_cassette("get_2d_spectrum_numeric_dates")

  start_unix <- as.numeric(as.POSIXct("1994-01-01", tz = "UTC"))
  end_unix <- as.numeric(as.POSIXct("1994-01-31", tz = "UTC"))

  spec <- get_2d_spectrum(
    "SEMREVO",
    start = start_unix,
    end = end_unix
  )

  expect_type(spec, "list")
  expect_gt(nrow(spec$forcings), 0)
})

test_that("get_2d_spectrum handles multi-month requests", {
  # vcr::local_cassette("get_2d_spectrum_multi_month")

  spec <- get_2d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-03-31"
  )

  expect_type(spec, "list")
  expect_gt(nrow(spec$forcings), 2000)
})

test_that("get_2d_spectrum validates forcings data structure", {
  # vcr::local_cassette("get_2d_spectrum_forcings_structure")

  spec <- get_2d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-01-31"
  )

  # Check forcings has expected columns
  expected_cols <- c("time", "dpt", "wnd", "wnddir", "cur", "curdir")
  expect_true(all(expected_cols %in% names(spec$forcings)))

  # Check time is POSIXct
  expect_s3_class(spec$forcings$time, "POSIXct")
})

test_that("get_2d_spectrum validates spectral array dimensions", {
  # vcr::local_cassette("get_2d_spectrum_array_dimensions")

  spec <- get_2d_spectrum(
    "SEMREVO",
    start = "1994-01-01",
    end = "1994-01-31"
  )

  # Check that spectral array has correct dimensions
  expect_true(is.array(spec$efth))
  expect_equal(length(dim(spec$efth)), 3)
  expect_equal(dim(spec$efth)[1], length(spec$dir))
  expect_equal(dim(spec$efth)[2], length(spec$freq))
  expect_equal(dim(spec$efth)[3], nrow(spec$forcings))
})

# Error handling tests - input validation (no network needed)
test_that("get_2d_spectrum validates point input length", {
  expect_error(
    get_2d_spectrum(
      c("SEMREVO", "AUTRE"),
      start = "1994-01-01",
      end = "1994-01-31"
    ),
    "length\\(point\\) == 1"
  )
})

test_that("get_2d_spectrum validates point exists", {
  expect_error(
    get_2d_spectrum(
      "INVALID_POINT",
      start = "1994-01-01",
      end = "1994-01-31"
    ),
    "point %in% resourcecodedata::rscd_spectral\\$name"
  )
})

test_that("get_2d_spectrum validates date range", {
  expect_error(
    get_2d_spectrum(
      "SEMREVO",
      start = "1994-01-31",
      end = "1994-01-01"
    ),
    "end >= start"
  )
})

test_that("get_2d_spectrum validates start date within coverage", {
  expect_error(
    get_2d_spectrum(
      "SEMREVO",
      start = "1980-01-01",
      end = "1994-01-31"
    ),
    "format\\(start, \"%Y\"\\) >= format\\(rscd_hindcast_start_date"
  )
})

test_that("get_2d_spectrum validates end date within coverage", {
  expect_error(
    get_2d_spectrum(
      "SEMREVO",
      start = "1994-01-01",
      end = "2030-01-01"
    ),
    "format\\(end, \"%Y\"\\) <= format\\(rscd_hindcast_end_date"
  )
})

# Network failure tests - using mocks (no vcr needed)
test_that("get_2d_spectrum fails gracefully when first download fails", {
  mockery::stub(
    get_2d_spectrum,
    "get_2d_spectrum_raw",
    NULL
  )

  expect_null(
    get_2d_spectrum(
      "SEMREVO",
      start = "1994-01-01",
      end = "1994-01-31"
    )
  )
})

# Edge case: Boundary dates
test_that("get_1d_spectrum accepts dates at exact boundaries", {
  # vcr::local_cassette("get_1d_spectrum_boundary_dates")

  # Test with start date at exact boundary
  expect_no_error(
    get_1d_spectrum(
      "SEMREVO",
      start = format(resourcecode:::rscd_hindcast_start_date, "%Y-01-01"),
      end = format(resourcecode:::rscd_hindcast_start_date, "%Y-01-31")
    )
  )
})

test_that("get_2d_spectrum accepts dates at exact boundaries", {
  # vcr::local_cassette("get_2d_spectrum_boundary_dates")

  # Test with start date at exact boundary
  expect_no_error(
    get_2d_spectrum(
      "SEMREVO",
      start = format(resourcecode:::rscd_hindcast_start_date, "%Y-01-01"),
      end = format(resourcecode:::rscd_hindcast_start_date, "%Y-01-31")
    )
  )
})
