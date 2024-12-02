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
get_parameters_raw <- function(parameter = "hs",
                               node = 42,
                               start = as.POSIXct("1994-01-01Z00:00:00"),
                               end = as.POSIXct("1994-12-31Z23:00:00")) {
  if (parameter == "tp") {
    single_parameter <- "fp"
  } else {
    single_parameter <- parameter
  }

  start_str <- strftime(start, format = "%Y-%m-%dT%H:%M:%SZ")
  end_str <- strftime(end, format = "%Y-%m-%dT%H:%M:%SZ")

  # Cassandra database start indexing at 1, so decrements node number
  node <- node - 1

  request <- paste0(
    rcd_cassandra_url, "/api/timeseries",
    "?parameter=", single_parameter,
    "&node=", node,
    "&start=", start_str,
    "&end=", end_str
  )

  res <- jsonlite::fromJSON(request)

  if (res$errorcode != 0) {
    stop(paste0(
      "Unable to get a response from the database.\nStatus code: ",
      res$errormessage
    ))
  }

  data <- res$result$data
  colnames(data) <- c("time", parameter)
  data <- tibble::as_tibble(data)


  if (parameter == "tp") {
    data[, 2] <- 1 / data[, 2]
  }

  data$time <- as.POSIXct(data$time / 1000,
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
#' @examplesIf requireNamespace("resourcecodedata", quietly = TRUE)
#' ts <- get_parameters(parameters = c("hs", "tp"), node = 42)
#' plot(ts$time, ts$hs, type = "l")
get_parameters <- function(parameters = "hs",
                           node = 42,
                           start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
                           end = as.POSIXct("1994-12-31 23:00:00", tz = "UTC")) {
  has_data()

  parameters <- tolower(parameters)

  stopifnot(all(parameters %in% c("tp", resourcecodedata::rscd_variables$name)))

  node <- as.integer(node)

  stopifnot(length(node) == 1)
  stopifnot(node %in% resourcecodedata::rscd_field$node)

  if (is.character(start)) {
    start <- as.POSIXct(start, tz = "UTC")
  }
  if (is.character(end)) {
    end <- as.POSIXct(end, tz = "UTC")
  }

  if (is.numeric(start)) {
    start <- as.POSIXct(start, tz = "UTC", origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
  }
  if (is.numeric(end)) {
    end <- as.POSIXct(end, tz = "UTC", origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
  }

  stopifnot(start >= rscd_casandra_start_date)
  stopifnot(end <= rscd_casandra_end_date)

  out <- get_parameters_raw(parameters[1], node = node, start = start, end = end)

  for (i in seq_len(length(parameters) - 1)) {
    temp <- get_parameters_raw(parameters[i + 1], node = node, start = start, end = end)
    out <- cbind(out, temp[, 2])
  }
  out
}
