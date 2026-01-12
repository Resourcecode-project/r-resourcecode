#' Helper function to download netCDF file given its URL
#'
#' @param url the remote file to download
#' @param destfile the destination file
#'
#' @returns the path to the destination file, or NULL if an error occurred.
#'
#' @noRd
#' @keywords internal
download_nc <- function(url, destfile) {
  tryCatch(
    {
      req <- httr2::request(url) |>
        httr2::req_timeout(60) |>
        httr2::req_error(is_error = ~ .x$status_code >= 400)

      httr2::req_perform(req, path = destfile)

      if (!file.exists(destfile)) {
        return(NULL)
      }

      destfile
    },
    error = function(e) {
      message("Download failed: ", conditionMessage(e))
      if (file.exists(destfile)) {
        file.remove(destfile)
      }
      NULL
    }
  )
}


#' Download the 2D spectrum data from IFREMER ftp for a single data.
#'
#' No consistency checks are made, this function should not be called directly by the user
#'
#' @param point the location name (string) requested.
#' @param year the year (as a string) requested.
#' @param month the month number, as a string,
#'
#' @return a list with the sea-state spectrum and forcings
#' @noRd
#' @keywords internal
get_2d_spectrum_raw <- function(point, year, month) {
  base <- "https://data-dataref.ifremer.fr/ww3/resourcecode/HINDCAST/"
  url <- paste0(
    base,
    year,
    "/",
    month,
    "/SPEC_NC/RSCD_WW3-RSCD-UG-",
    point,
    "_",
    year,
    month,
    "_spec.nc"
  )

  temp <- tempfile(fileext = ".nc")

  file <- download_nc(url, temp)

  if (is.null(file)) {
    message(
      "Could not download spectral data.
              The remote server may be unavailable or the URL may have changed."
    )
    return(NULL)
  }

  nc <- ncdf4::nc_open(file)

  on.exit({
    ncdf4::nc_close(nc)
    file.remove(temp)
  })

  freq <- nc$dim$frequency$vals
  dir <- nc$dim$direction$vals
  dir_ordred <- order(dir)

  time <- nc$dim$time$vals
  time <- as.POSIXct(
    time * 24 * 3600,
    origin = "1990-01-01Z00:00:00",
    tz = "UTC"
  )

  var_out <- list("longitude", "latitude", "frequency1", "frequency2", "efth")
  out <- lapply(var_out, ncdf4::ncvar_get, nc = nc)
  names(out) <- var_out

  out$longitude <- out$longitude[1]
  out$latitude <- out$latitude[1]
  out$freq <- freq
  out$dir <- dir[dir_ordred]

  var_out_1d <- list("dpt", "wnd", "wnddir", "cur", "curdir")
  forcings <- lapply(var_out_1d, ncdf4::ncvar_get, nc = nc)
  names(forcings) <- var_out_1d

  out$forcings <- tibble::tibble(time = time, tibble::as_tibble(forcings))

  out$efth <- 10^(out$efth) - 1e-12
  out$efth <- out$efth[dir_ordred, , ]

  out$station <- point
  out
}

#' Download the 1D spectrum data from IFREMER ftp for a single data.
#'
#' No consistency checks are made, this function should not be called directly by the user
#'
#' @param point the location name (string) requested.
#' @param year the year (as a string) requested.
#' @param month the month number, as a string,
#'
#' @return a list with the sea-state 1D spectrum and forcings
#' @noRd
#' @keywords internal
get_1d_spectrum_raw <- function(point, year, month) {
  base <- "https://data-dataref.ifremer.fr/ww3/resourcecode/HINDCAST/"
  url <- paste0(
    base,
    year,
    "/",
    month,
    "/FREQ_NC/RSCD_WW3-RSCD-UG-",
    point,
    "_",
    year,
    month,
    "_freq.nc"
  )
  temp <- tempfile(fileext = ".nc")

  file <- download_nc(url, temp)

  if (is.null(file)) {
    message(
      "Could not download spectral data.
              The remote server may be unavailable or the URL may have changed."
    )
    return(NULL)
  }

  nc <- ncdf4::nc_open(file)

  on.exit({
    ncdf4::nc_close(nc)
    file.remove(temp)
  })

  freq <- nc$dim$frequency$vals

  time <- nc$dim$time$vals
  time <- as.POSIXct(
    time * 24 * 3600,
    origin = "1990-01-01Z00:00:00",
    tz = "UTC"
  )

  var_out <- list(
    "longitude",
    "latitude",
    "frequency1",
    "frequency2",
    "ef",
    "th1m",
    "th2m",
    "sth1m",
    "sth2m"
  )
  out <- lapply(var_out, ncdf4::ncvar_get, nc = nc)
  names(out) <- var_out

  out$longitude <- out$longitude[1]
  out$latitude <- out$latitude[1]
  out$freq <- freq

  var_out_1d <- list(
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
  forcings <- lapply(var_out_1d, ncdf4::ncvar_get, nc = nc)

  names(forcings) <- var_out_1d

  out$forcings <- tibble::tibble(time = time, tibble::as_tibble(forcings))

  out$station <- point
  out
}

#' Download the 2D spectrum data from IFREMER ftp
#'
#' @param point the location name (string) requested.
#'              Alternatively, the node number. The consistency is checked internally.
#' @param start the starting date (as a string). The consistency is checked internally.
#' @param end the ending date as a string
#'
#' @return A list with 9 elements:
#' \describe{
#'   \item{longitude}{Longitude}
#'   \item{latitude}{Latitude}
#'   \item{frequency1}{Lower frequency}
#'   \item{frequency2}{Upper frequency}
#'   \item{ef}{Surface elevation variance spectral density}
#'   \item{th1m}{Mean direction from first spectral moment}
#'   \item{th2m}{Mean direction from second spectral moment}
#'   \item{sth1m}{Mean directional spreading from first spectral moment}
#'   \item{sth2m}{Mean directional spreading from second spectral moment}
#'   \item{freq}{Central frequency}
#'   \item{dir}{Directionnal bins}
#'   \item{forcings}{A data.frame with 6 variables:
#'    \describe{
#'      \item{time}{Time}
#'      \item{dpt}{Depth, positive downward}
#'      \item{wnd}{Wind intensity, at 10m above sea level}
#'      \item{wnddir}{Wind direction, comes from}
#'      \item{cur}{Current intensity, at the surface}
#'      \item{curdir}{Current direction, going to}
#'    }}
#'   \item{station}{Station name}
#' }
#' @export
#'
#' @examples
#' spec2D <- get_2d_spectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
#' if(!is.null(spec2D)){
#'   image(spec2D$dir, spec2D$freq, spec2D$efth[, , 1],
#'     xlab = "Direction (°)",
#'     ylab = "Frequency (Hz"
#'   )
#' }
get_2d_spectrum <- function(point, start = "1994-01-01", end = "1994-02-28") {
  stopifnot(length(point) == 1)

  if (is.numeric(point)) {
    point <- resourcecodedata::rscd_spectral[point, "name"]
  }

  stopifnot(point %in% resourcecodedata::rscd_spectral$name)

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

  stopifnot(format(start, "%Y") >= format(rscd_hindcast_start_date, "%Y"))
  stopifnot(format(end, "%Y") <= format(rscd_hindcast_end_date, "%Y"))
  stopifnot(end >= start)

  dates <- seq.POSIXt(from = start, to = end, by = "month")
  years <- format(dates, format = "%Y")
  months <- format(dates, format = "%m")

  out <- get_2d_spectrum_raw(point, years[1], months[1])

  if (is.null(out)) {
    message(
      "Failed to download data for ",
      point,
      " (",
      years[1],
      "-",
      months[1],
      ")"
    )
    return(NULL)
  }

  for (m in seq_along(years[-1])) {
    temp <- get_2d_spectrum_raw(point, years[m + 1], months[m + 1])

    if (is.null(temp)) {
      message(
        "Failed to download data for ",
        point,
        " (",
        years[m + 1],
        "-",
        months[m + 1],
        ")"
      )
      return(NULL)
    }

    out$efth <- abind::abind(out$efth, temp$efth, along = 3)
    out$forcings <- rbind(out$forcings, temp$forcings)
  }

  out
}


#' Download the 1D spectrum data from IFREMER ftp
#'
#' @param point the location name (string) requested.
#'              Alternatively, the node number. The consistency is checked internally.
#' @param start the starting date (as a string). The consistency is checked internally.
#' @param end the ending date as a string
#'
#' @return A list with 12 elements:
#' \describe{
#'   \item{longitude}{Longitude}
#'   \item{latitude}{Latitude}
#'   \item{frequency1}{Lower frequency}
#'   \item{frequency2}{Upper frequency}
#'   \item{ef}{Surface elevation variance spectral density}
#'   \item{th1m}{Mean direction from first spectral moment}
#'   \item{th2m}{Mean direction from second spectral moment}
#'   \item{sth1m}{Mean directional spreading from first spectral moment}
#'   \item{sth2m}{Mean directional spreading from second spectral moment}
#'   \item{freq}{Central frequency}
#'   \item{forcings}{A data.frame with 14 variables:
#'    \describe{
#'      \item{time}{Time}
#'      \item{dpt}{Depth, positive downward}
#'      \item{wnd}{Wind intensity, at 10m above sea level}
#'      \item{wnddir}{Wind direction, comes from}
#'      \item{cur}{Current intensity, at the surface}
#'      \item{curdir}{Current direction, going to}
#'      \item{hs}{Significant wave height}
#'      \item{fp}{Peak wave frequency}
#'      \item{f02}{Mean wave frequency}
#'      \item{f0m1}{Mean wave frequency at spectral moment minus one}
#'      \item{th1p}{Mean wave direction at spectral peak}
#'      \item{sth1p}{Directional spreading at spectral peak}
#'      \item{dir}{Mean wave direction}
#'      \item{spr}{Mean directional spreading}
#'    }}
#'   \item{station}{Station name}
#' }
#' @export
#'
#' @examples
#' spec1D <- get_1d_spectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
#' if(!is.null(spec1D)){
#'   r <- as.POSIXct(round(range(spec1D$forcings$time), "month"))
#'   image(spec1D$forcings$time, spec1D$freq, t(spec1D$ef),
#'     xaxt = "n", xlab = "Time",
#'     ylab = "Frequency (Hz)"
#'   )
#'   axis.POSIXct(1, spec1D$forcings$time,
#'     at = seq(r[1], r[2], by = "week"),
#'     format = "%Y-%m-%d", las = 2
#'   )
#' }
get_1d_spectrum <- function(point, start = "1994-01-01", end = "1994-02-28") {
  stopifnot(length(point) == 1)

  if (is.numeric(point)) {
    point <- resourcecodedata::rscd_spectral[point, "name"]
  }

  stopifnot(point %in% resourcecodedata::rscd_spectral$name)

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
      origin = as.POSIXct("1970-01-01", tz = "UTC")
    )
  }
  if (is.numeric(end)) {
    end <- as.POSIXct(
      end,
      tz = "UTC",
      origin = as.POSIXct("1970-01-01", tz = "UTC")
    )
  }

  stopifnot(format(start, "%Y") >= format(rscd_hindcast_start_date, "%Y"))
  stopifnot(format(end, "%Y") <= format(rscd_hindcast_end_date, "%Y"))
  stopifnot(end >= start)

  dates <- seq.POSIXt(from = start, to = end, by = "month")
  years <- format(dates, format = "%Y")
  months <- format(dates, format = "%m")

  out <- get_1d_spectrum_raw(point, years[1], months[1])

  if (is.null(out)) {
    message(
      "Failed to download data for ",
      point,
      " (",
      years[1],
      "-",
      months[1],
      ")"
    )
    return(NULL)
  }

  for (m in seq_along(years[-1])) {
    temp <- get_1d_spectrum_raw(point, years[m + 1], months[m + 1])

    if (is.null(temp)) {
      message(
        "Failed to download data for ",
        point,
        " (",
        years[m + 1],
        "-",
        months[m + 1],
        ")"
      )
      return(NULL)
    }

    out$ef <- abind::abind(out$ef, temp$ef, along = 2)
    out$th1m <- abind::abind(out$th1m, temp$th1m, along = 2)
    out$th2m <- abind::abind(out$th2m, temp$th2m, along = 2)
    out$sth1m <- abind::abind(out$sth1m, temp$sth1m, along = 2)
    out$sth2m <- abind::abind(out$sth2m, temp$sth2m, along = 2)
    out$forcings <- rbind(out$forcings, temp$forcings)
  }

  out
}
