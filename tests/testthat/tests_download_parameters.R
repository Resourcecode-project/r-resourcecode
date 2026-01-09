# Configure vcr for your tests
# vcr_configure(
#   dir = "tests/fixtures/vcr_cassettes",
#   filter_sensitive_data = list(
#     # If you have API keys or sensitive data in URLs, filter them here
#     # "<<<MY_SECRET>>>" = Sys.getenv("SECRET_KEY")
#   )
# )

#Tests for get_parameters function (which also tests get_parameters_raw internally)
test_that("get_parameters retrieves single parameter and tests basic functionality", {
  vcr::local_cassette("get_single_parameter")
  result <- get_parameters(
    parameters = "hs",
    node = 42,
    start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("time", "hs"))
  expect_s3_class(result$time, "POSIXct")
  expect_type(result$hs, "double")
  expect_true(nrow(result) > 0)
})

test_that("get_parameters retrieves multiple parameters including tp conversion", {
  vcr::local_cassette("get_multiple_parameters")
  result <- get_parameters(
    parameters = c("hs", "tp"),
    node = 42,
    start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("time", "hs", "tp"))
  expect_true(nrow(result) > 0)
  # Check that tp values are positive (tests fp to tp conversion in get_parameters_raw)
  expect_true(all(result$tp > 0, na.rm = TRUE))
})

test_that("get_parameters handles character date inputs", {
  vcr::local_cassette("character_dates")
  result <- get_parameters(
    parameters = "hs",
    node = 42,
    start = "1994-01-01 00:00:00",
    end = "1994-01-02 00:00:00"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("get_parameters handles numeric date inputs", {
  vcr::local_cassette("numeric_dates")
  start_num <- as.numeric(as.POSIXct("1994-01-01 00:00:00", tz = "UTC"))
  end_num <- as.numeric(as.POSIXct("1994-01-02 00:00:00", tz = "UTC"))

  result <- get_parameters(
    parameters = "hs",
    node = 42,
    start = start_num,
    end = end_num
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# Error handling tests (these don't need vcr as they fail before API call)
test_that("get_parameters validates parameter names", {
  expect_error(
    get_parameters(
      parameters = c("hs", "invalid_param"),
      node = 42
    ),
    "Requested parameters do not exists"
  )
})

test_that("get_parameters validates node input", {
  expect_error(
    get_parameters(
      parameters = "hs",
      node = c(42, 43)
    ),
    "only one location a time"
  )
})

test_that("get_parameters validates date range", {
  expect_error(
    get_parameters(
      parameters = "hs",
      node = 42,
      start = as.POSIXct("1994-01-02 00:00:00", tz = "UTC"),
      end = as.POSIXct("1994-01-01 00:00:00", tz = "UTC")
    ),
    "'end' must be after 'start'"
  )
})

# Test with recorded fixtures to ensure consistent behavior
test_that("get_parameters produces expected data structure over time range", {
  vcr::local_cassette("week_of_data")
  result <- get_parameters(
    parameters = c("hs", "tp"),
    node = 42,
    start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("1994-01-07 23:00:00", tz = "UTC")
  )

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3) # time, hs, tp
  # Expect roughly hourly data for a week (168 hours)
  expect_gt(nrow(result), 100)
  expect_lt(nrow(result), 200)

  # Check data types
  expect_s3_class(result$time, "POSIXct")
  expect_type(result$hs, "double")
  expect_type(result$tp, "double")
})

# Error handling tests using mockery
# test_that("get_parameters_raw handles network timeout", {
#   mockery::stub(
#     get_parameters_raw,
#     "httr2::req_perform",
#     stop("Timeout was reached")
#   )
#
#   expect_message(
#     result <- get_parameters_raw(
#       parameter = "hs",
#       node = 42,
#       start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
#       end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
#     ),
#     "Network error"
#   )
#
#   expect_null(result)
# })

test_that("get_parameters_raw handles HTTP 404 error", {
  mock_resp <- structure(
    list(status_code = 404),
    class = "httr2_response"
  )

  mockery::stub(get_parameters_raw, "httr2::req_perform", mock_resp)
  mockery::stub(get_parameters_raw, "httr2::resp_status", 404)
  mockery::stub(get_parameters_raw, "httr2::resp_status_desc", "Not Found")

  expect_message(
    result <- get_parameters_raw(
      parameter = "hs",
      node = 42,
      start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
      end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
    ),
    "HTTP error 404"
  )

  expect_null(result)
})

test_that("get_parameters_raw handles HTTP 500 server error", {
  mock_resp <- structure(
    list(status_code = 500),
    class = "httr2_response"
  )

  mockery::stub(get_parameters_raw, "httr2::req_perform", mock_resp)
  mockery::stub(get_parameters_raw, "httr2::resp_status", 500)
  mockery::stub(
    get_parameters_raw,
    "httr2::resp_status_desc",
    "Internal Server Error"
  )

  expect_message(
    result <- get_parameters_raw(
      parameter = "hs",
      node = 42,
      start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
      end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
    ),
    "HTTP error 500"
  )

  expect_null(result)
})

# test_that("get_parameters_raw handles invalid JSON response", {
#   mock_resp <- structure(
#     list(status_code = 200),
#     class = "httr2_response"
#   )
#
#   mockery::stub(get_parameters_raw, "httr2::req_perform", mock_resp)
#   mockery::stub(get_parameters_raw, "httr2::resp_status", 200)
#   mockery::stub(
#     get_parameters_raw,
#     "httr2::resp_body_json",
#     stop("Invalid JSON")
#   )
#
#   expect_message(
#     result <- get_parameters_raw(
#       parameter = "hs",
#       node = 42,
#       start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
#       end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
#     ),
#     "Error parsing response"
#   )
#
#   expect_null(result)
# })

test_that("get_parameters_raw handles API-level error in response", {
  mock_resp <- structure(
    list(status_code = 200),
    class = "httr2_response"
  )

  mock_json <- list(
    errorcode = 1,
    errormessage = "Invalid node parameter"
  )

  mockery::stub(get_parameters_raw, "httr2::req_perform", mock_resp)
  mockery::stub(get_parameters_raw, "httr2::resp_status", 200)
  mockery::stub(get_parameters_raw, "httr2::resp_body_json", mock_json)

  expect_message(
    result <- get_parameters_raw(
      parameter = "hs",
      node = 42,
      start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
      end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
    ),
    "Invalid node parameter"
  )

  expect_null(result)
})

test_that("get_parameters handles failure in get_parameters_raw for single parameter", {
  # Mock get_parameters_raw to return NULL (simulating any failure)
  mockery::stub(get_parameters, "get_parameters_raw", NULL)

  expect_message(
    result <- get_parameters(
      parameters = "hs",
      node = 42,
      start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
      end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
    ),
    "Failed to retrieve parameter: hs"
  )

  expect_null(result)
})

test_that("get_parameters handles partial failure with multiple parameters", {
  # First call succeeds, second call fails
  mock_success <- tibble::tibble(
    time = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
    hs = 1.5
  )

  mockery::stub(
    get_parameters,
    "get_parameters_raw",
    mockery::mock(mock_success, NULL, cycle = TRUE)
  )

  expect_message(
    result <- get_parameters(
      parameters = c("hs", "tp"),
      node = 42,
      start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
      end = as.POSIXct("1994-01-02 00:00:00", tz = "UTC")
    ),
    "Failed to retrieve parameter: tp"
  )

  expect_null(result)
})


test_that("Errors in 'get_parameters()' are handled correcly", {
  expect_error(
    get_parameters("tépé"),
    "Requested parameters do not exists in the database: tépé"
  )
  expect_error(
    get_parameters(node = 0),
    "The requested location do no exist in the database."
  )
  expect_error(
    get_parameters(node = c(10, 100)),
    "The function can retreive only one location a time."
  )
  expect_error(
    get_parameters(start = 1),
    paste0(
      "'start' is outside the covered period: ",
      paste(
        format(
          c(
            resourcecode:::rscd_casandra_start_date,
            resourcecode:::rscd_casandra_end_date
          ),
          format = "%Y-%m-%d %H:%M %Z"
        ),
        collapse = " \u2014 "
      )
    )
  )
  expect_error(
    get_parameters(end = 1e10),
    paste0(
      "'end' is outside the covered period: ",
      paste(
        format(
          c(
            resourcecode:::rscd_casandra_start_date,
            resourcecode:::rscd_casandra_end_date
          ),
          format = "%Y-%m-%d %H:%M %Z"
        ),
        collapse = " \u2014 "
      )
    )
  )
  expect_error(
    get_parameters(
      start = "1994-01-31 01:00:00",
      end = "1994-01-11 01:00:00"
    ),
    "'end' must be after 'start'"
  )
})
