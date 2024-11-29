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
