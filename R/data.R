#' Resourcecode frequency vector of 1D and 2D spectra
#'
#' The wave spectrum discretization considers 36 frequencies, starting from
#' 0.0339 Hz up to 0.9526 Hz with a frequency increment factor of 1.1
#'
#' @format ## `rscd_freq`
#' An array 36 elements with the frequencies values
#'
#' @source User Manual of the RESOURCECODE database
#' <https://archimer.ifremer.fr/doc/00751/86306/>
"rscd_freq"

#' Resourcecode directional bins
#'
# In terms of directional discretization, 36 directions were used
#' (equivalent to a directional resolution of 10°;
#'
#' @format ## `rscd_dir`
#' An array of length 36 with the directionnal bins
#'
#' @source User Manual of the RESOURCECODE database
#' <https://archimer.ifremer.fr/doc/00751/86306/>
"rscd_dir"

#' Resourcecode data extract
#'
#' An extract of the Resourcecode Hindcast database, at node 123456, spanning from
#'  1994-01-01 00:00:00 to 1999-12-31 23:00:00.
#'
#'@format ## A data frame with 87,648 rows and 7 variables:
#' \describe{
#'   \item{time}{POSIXct. Timestamp in UTC (hourly resolution).}
#'   \item{hs}{numeric. Significant wave height (m).}
#'   \item{tp}{numeric. Peak wave period (s).}
#'   \item{dp}{numeric. Mean wave direction (degrees, coming from true North, clockwise).}
#'   \item{uwnd}{numeric. Zonal (east–west) component of the 10-m wind (m/s). Positive eastward.}
#'   \item{vwnd}{numeric. Meridional (north–south) component of the 10-m wind (m/s).
#'   Positive northward.}
#'   \item{dpt}{numeric.Depth (m).}
#' }
#'
#' @source User Manual of the RESOURCECODE database
#' <https://archimer.ifremer.fr/doc/00751/86306/>
"rscd_data_example"
