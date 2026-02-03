#' Get time series of sea-state parameters
#'
#' No verification is made within this function, and thus it should not
#' be called directly by the user
#'
#' @param parameter variable to fetch from the database
#' @param node single integer with the node to get
#' @param start starting date (posixct)
#' @param end end date (posixct)
#'
#' @return a tibble with 2 columns and as many rows as needed
#' @noRd
get_parameters_raw <- function(
  parameter = "hs",
  node = 42,
  start = as.POSIXct("1994-01-01Z00:00:00", tz = "UTC"),
  end = as.POSIXct("1994-12-31Z23:00:00", tz = "UTC")
) {
  if (parameter == "tp") {
    single_parameter <- "fp"
  } else {
    single_parameter <- parameter
  }

  start_str <- strftime(start, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_str <- strftime(end, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # Cassandra database start indexing at 1, so decrements node number
  node <- node - 1

  request_url <- paste0(
    rcd_cassandra_url,
    "api/timeseries",
    "?parameter=",
    single_parameter,
    "&node=",
    node,
    "&start=",
    start_str,
    "&end=",
    end_str
  )

  # Try retrieving and parsing JSON using httr2
  resp <- tryCatch(
    httr2::request(request_url) |>
      httr2::req_error(is_error = \(resp) FALSE) |> # Don't auto-error on HTTP errors
      httr2::req_retry(max_tries = 3) |> # Retry transient failures
      httr2::req_timeout(30) |> # 30 second timeout
      httr2::req_user_agent("Resourcecode R package") |>
      httr2::req_perform(),
    httr2_failure = function(cnd) {
      message(
        "Network error: Could not connect to the remote resource. ",
        "The server may be unavailable."
      )
      NULL
    },
    error = function(e) {
      message("Unexpected error retrieving data: ", conditionMessage(e))
      NULL
    }
  )

  # If request failed, exit
  if (is.null(resp)) {
    return(NULL)
  }

  # Check HTTP status
  if (httr2::resp_status(resp) != 200) {
    message(
      "HTTP error ",
      httr2::resp_status(resp),
      ": ",
      httr2::resp_status_desc(resp)
    )
    return(NULL)
  }

  # Parse JSON response
  res <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    error = function(e) {
      message("Error parsing response: Invalid JSON format")
      NULL
    }
  )

  # If parsing failed, exit
  if (is.null(res)) {
    return(NULL)
  }

  # Check API-level error
  if (!is.null(res$errorcode) && res$errorcode != 0) {
    message(
      "The data source returned an error: ",
      res$errormessage,
      "\nReturning NULL."
    )
    return(NULL) # graceful fallback
  }

  # Convert list to data frame
  data <- as.data.frame(res$result$data)
  colnames(data) <- c("time", parameter)
  data <- tibble::as_tibble(data)

  if (parameter == "tp") {
    data[, 2] <- 1 / data[, 2]
  }

  data$time <- as.POSIXct(
    as.numeric(data$time) / 1000,
    origin = as.POSIXct("1970-01-01", tz = "UTC"),
    tz = "UTC"
  ) # Convert UNIX time (ms) to POSIXct format
  attr(data, "node") <- node
  data
}


#' Download time series of sea-state parameters from RESOURCECODE database
#'
#' If the remote resource is unavailable or returns an error, the function returns NULL
#' and emits an informative message.
#'
#' @param parameters character vector of sea-state parameters
#' @param node single integer with the node to get
#' @param start starting date (as integer, character or posixct)
#' @param end ending date (as integer, character or posixct)
#'
#' @return a tibble with N-rows and `length(parameters)` columns.
#' @export
#'
#' @examples
#' rscd_data <- get_parameters(parameters = c("hs", "tp"), node = 42)
#' if(!is.null(rscd_data)) plot(rscd_data$time, rscd_data$hs, type = "l")
get_parameters <- function(
  parameters = "hs",
  node = 42,
  start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
  end = as.POSIXct("1994-12-31 23:00:00", tz = "UTC")
) {
  parameters <- tolower(parameters)

  if (any(parameters %nin% c("tp", resourcecodedata::rscd_variables$name))) {
    errors <- parameters[
      parameters %nin% c("tp", resourcecodedata::rscd_variables$name)
    ]
    stop(
      "Requested parameters do not exists in the database: ",
      paste0(errors, collapse = ", "),
      "."
    )
  }

  node <- as.integer(node)

  if (length(node) != 1) {
    stop("The function can retreive only one location a time.")
  }
  if (node %nin% resourcecodedata::rscd_field$node) {
    stop("The requested location do no exist in the database.")
  }

  if (is.character(start)) {
    start <- as.POSIXct(start, tz = "UTC")
  }
  if (is.character(end)) {
    end <- as.POSIXct(end, tz = "UTC")
  }

  if (is.numeric(start)) {
    start <- as.POSIXct(
      start,
      tz = "UTC",
      origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
    )
  }
  if (is.numeric(end)) {
    end <- as.POSIXct(
      end,
      tz = "UTC",
      origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
    )
  }

  if (start < rscd_casandra_start_date) {
    stop(
      "'start' is outside the covered period: ",
      paste(
        format(
          c(rscd_casandra_start_date, rscd_casandra_end_date),
          format = "%Y-%m-%d %H:%M %Z"
        ),
        collapse = " \u2014 "
      )
    )
  }
  if (end > rscd_casandra_end_date) {
    stop(
      "'end' is outside the covered period: ",
      paste(
        format(
          c(rscd_casandra_start_date, rscd_casandra_end_date),
          format = "%Y-%m-%d %H:%M %Z"
        ),
        collapse = " \u2014 "
      )
    )
  }
  if (start >= end) {
    stop("'end' must be after 'start'")
  }

  out <- get_parameters_raw(
    parameters[1],
    node = node,
    start = start,
    end = end
  )

  # If first parameter retrieval failed, return NULL
  if (is.null(out)) {
    message("Failed to retrieve parameter: ", parameters[1])
    return(NULL)
  }

  for (i in seq_len(length(parameters) - 1)) {
    temp <- get_parameters_raw(
      parameters[i + 1],
      node = node,
      start = start,
      end = end
    )

    # If any subsequent parameter retrieval fails, return NULL
    if (is.null(temp)) {
      message("Failed to retrieve parameter: ", parameters[i + 1])
      return(NULL)
    }

    out <- cbind.data.frame(out, temp[, 2])
  }
  out
}
