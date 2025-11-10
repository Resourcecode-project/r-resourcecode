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

test_that("downloading parameters data works", {
  skip_if_offline()
  dat <- get_parameters(
    parameters = c("hs", "tp"),
    node = 42,
    start = "1994-01-01 00:00:00 UTC",
    end = 760057200
  )
  expect_s3_class(dat, "data.frame")
  expect_equal(names(dat), c("time", "hs", "tp"))
  expect_equal(NROW(dat), 24 * 31)
  expect_equal(
    get_parameters(
      parameters = c("hs", "tp"),
      node = 42,
      start = 757382400,
      end = "1994-01-31 23:00:00 UTC"
    ),
    dat
  )
})

test_that("get_parameters_raw() returns NULL and message on failure", {

  get_parameters_raw <- getFromNamespace("get_parameters_raw", "resourcecode")
  # mock a function that throws error (as if network/API failed)
  mock_fromJSON <- function(...) stop("network failure") # nolint

  # temporarily replace fromJSON inside your function
  mockery::stub(get_parameters_raw, "jsonlite::fromJSON", mock_fromJSON)

  expect_message(
    result <- get_parameters_raw("hs"),
    "Could not retrieve data"
  )

  expect_null(result)
})

test_that("get_parameters_raw() handles API-side error codes", {

  get_parameters_raw <- getFromNamespace("get_parameters_raw", "resourcecode")
  fake_api_response <- list(errorcode = 123, errormessage = "Invalid request")

  mock_fromJSON <- function(...) fake_api_response # nolint

  mockery::stub(get_parameters_raw, "jsonlite::fromJSON", mock_fromJSON)

  expect_message(
    result <- get_parameters_raw("anything"),
    "The data source returned an error"
  )

  expect_null(result)
})

test_that("downloading 1D spectral data works", {
  skip_if_offline()
  spec <- get_1d_spectrum(
    1L,
    start = "1994-12-01 00:00:00 UTC",
    end = "1995-01-31 00:00:00 UTC"
  )
  expect_equal(spec, get_1d_spectrum(1L, start = 786243600, end = 791506800))
  expect_type(spec, "list")
  expect_equal(
    names(spec),
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
  expect_equal(NROW(spec$forcings), 1488)
  expect_equal(spec$station, "E001500N52000")
})


test_that("downloading 2D spectral data works", {
  skip_if_offline()
  spec <- get_2d_spectrum(
    1L,
    start = "1994-12-01 00:00:00 UTC",
    end = "1995-01-31 00:00:00 UTC"
  )
  expect_equal(spec, get_2d_spectrum(1L, start = 786243600, end = 791506800))
  expect_type(spec, "list")
  expect_equal(
    names(spec),
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
  expect_equal(NROW(spec$forcings), 1488)
  expect_equal(spec$station, "E001500N52000")
})
