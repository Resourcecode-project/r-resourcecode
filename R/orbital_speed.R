#' Compute the orbital speed at a given depth from the wave elevation 1D spectra
#'
#' @param spec 1D spectral data: TxM matrix
#' @param freq the M frequencies
#' @param z distance above the floor at which we want the orbital speed (single numeric)
#' @param depth depth time series (vector length T. Recycled if a single value is given)
#' @param output_speeds TRUE if the spectral speed are needed. Otherwise, returns the RMS (default)
#'
#' @return depending on spec, a list with the spectral velocities for each component
#'         (if output_speeds==FALSE) or a data.frame with a time series of horizontal and vertical
#'         components of (spectral) orbital speed.
#' @export
#'
#' @examples
#' # Compute orbital speed for varying Hs
#' S <- t(sapply(1:10, function(h) {
#'   jonswap(h)$spec
#' }))
#' orb_speeds <- compute_orbital_speeds(S, rscd_freq, depth = 100, z = 10)
#' plot(1:10, orb_speeds[, 1],
#'   type = "l",
#'   ylim = range(orb_speeds),
#'   xlab = "Hs (m)",
#'   ylab = "Orbital speed RMS (m/s)"
#' )
#' lines(1:10, orb_speeds[, 2], type = "l", col = "red")
compute_orbital_speeds <- function(
    spec,
    freq,
    z = 0,
    depth = Inf,
    output_speeds = FALSE) {
  # z: distance above sea floor

  dims <- dim(spec)
  n_time <- dims[1]
  n_freq <- dims[2]

  if (length(depth) == 1) {
    depth <- rep(depth, n_time)
  }

  stopifnot(n_freq == length(freq))
  stopifnot(z >= 0)
  stopifnot(z <= depth)

  # Compute k efficiently when depth is discretized
  k <- outer(
    freq,
    unique(depth),
    Vectorize(
      resourcecode::dispersion,
      vectorize.args = c("frequencies", "depth")
    )
  )
  mat_k <- t(k[, match(depth, unique(depth))])

  mat_d <- matrix(depth, nrow = n_time, ncol = n_freq)
  mat_depth <- matrix(z, nrow = n_time, ncol = n_freq)
  mat_freq <- matrix(freq, nrow = n_time, ncol = n_freq, byrow = TRUE)

  csh1 <- cosh(mat_k * mat_depth)
  ssh1 <- sinh(mat_k * mat_depth)
  ssh2 <- sinh(mat_k * mat_d)

  spectral_u_component <- (2 * pi * mat_freq * csh1 / ssh2)^2 * spec

  spectral_v_component <- (2 * pi * mat_freq * ssh1 / ssh2)^2 * spec

  if (output_speeds) {
    out <- array(NA, dim = c(dim(spectral_u_component), 2))
    out[, , 1] <- spectral_u_component
    out[, , 2] <- spectral_v_component
    return(out)
  } else {
    u_rms <- sqrt(2 * resourcecode::fastTrapz(freq, spectral_u_component, 2))
    v_rms <- sqrt(2 * resourcecode::fastTrapz(freq, spectral_v_component, 2))
    return(cbind(u_rms, v_rms))
  }
}
