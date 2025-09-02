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

  request <- paste0(
    rcd_cassandra_url,
    "/api/timeseries",
    "?parameter=",
    single_parameter,
    "&node=",
    node,
    "&start=",
    start_str,
    "&end=",
    end_str
  )

  res <- jsonlite::fromJSON(request)

  if (res$errorcode != 0) {
    stop(
      "Unable to get a response from the database.\nStatus code: ",
      res$errormessage
    )
  }

  data <- res$result$data
  colnames(data) <- c("time", parameter)
  data <- tibble::as_tibble(data)

  if (parameter == "tp") {
    data[, 2] <- 1 / data[, 2]
  }

  data$time <- as.POSIXct(
    data$time / 1000,
    origin = as.POSIXct("1970-01-01", tz = "UTC"),
    tz = "UTC"
  ) # Convert UNIX time (ms) to POSIXct format
  attr(data, "node") <- node
  data
}

#' Download time series of sea-state parameters from RESOURCECODE database
#'
#' @param parameters character vector of sea-state parameters
#' @param node single integer with the node to get
#' @param start starting date (as integer, character or posixct)
#' @param end ending date (as integer, character or posixct)
#'
#' @return a tibble with N-rows and `length(parameters)` columns.
#' @export
#'
#' @examplesIf curl::has_internet()
#' ts <- get_parameters(parameters = c("hs", "tp"), node = 42)
#' plot(ts$time, ts$hs, type = "l")
get_parameters <- function(
  parameters = "hs",
  node = 42,
  start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
  end = as.POSIXct("1994-12-31 23:00:00", tz = "UTC")
) {

  parameters <- tolower(parameters)

  if (any(parameters %nin% c("tp", resourcecodedata::rscd_variables$name))) {
    errors <- parameters[parameters %nin% c("tp", resourcecodedata::rscd_variables$name)]
    stop("Requested parameters do not exists in the database: ",
         paste0(errors, collapse = ", "), ".")
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
    stop("'start' is outside the covered period: ",
         paste(format(c(rscd_casandra_start_date, rscd_casandra_end_date),
                      format = "%Y-%m-%d %H:%M %Z"),
               collapse = " \u2014 "))
  }
  if (end > rscd_casandra_end_date) {
    stop("'end' is outside the covered period: ",
         paste(format(c(rscd_casandra_start_date, rscd_casandra_end_date),
                      format = "%Y-%m-%d %H:%M %Z"),
               collapse = " \u2014 "))
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

  for (i in seq_len(length(parameters) - 1)) {
    temp <- get_parameters_raw(
      parameters[i + 1],
      node = node,
      start = start,
      end = end
    )
    out <- cbind(out, temp[, 2])
  }
  out
}
