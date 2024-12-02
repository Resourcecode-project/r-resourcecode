#' Compute the dispersion relation of waves
#'  Find *k* s.t. (2.pi.f)^2 = g.k.tanh(k.d)
#'
#' @param frequencies frequency vector
#' @param depth depth (m)
#' @param iter_max maximum number of iterations in the solver
#' @param tol tolerance for termination.
#'
#' @return the wave numbers (same size as frequencies)
#' @export
#'
#' @examples
#' freq <- seq(from = 0, to = 1, length.out = 100)
#' k1 <- dispersion(freq, depth = 1)
#' k10 <- dispersion(freq, depth = 10)
#' kInf <- dispersion(freq, depth = Inf)
#' plot(freq, k1, type = "l")
#' lines(freq, k10, col = "red")
#' lines(freq, kInf, col = "green")
dispersion <- function(frequencies,
                       depth,
                       iter_max = 200,
                       tol = 1e-6) {
  g <- 9.81
  infinite_depth_dispersion <- (4 * pi ^ 2 / g) * frequencies ^ 2

  if (is.infinite(depth)) {
    return(infinite_depth_dispersion)
  }

  frequencies <- as.vector(frequencies)
  out <- frequencies

  for (f in seq_along(frequencies)) {
    if (frequencies[f] == 0) {
      out[f] <- 0
    } else {
      c0 <- (2 * pi * frequencies[f]) ^ 2
      k0 <- 4.0243 * frequencies[f] ^ 2
      xk <- k0
      ftest <- 99
      for (ii in 1:iter_max) {
        z <- xk * depth
        y <- tanh(z)
        ff <- c0 - g * xk * y
        dff <- g * (z * (y ^ 2 - 1) - y)
        xk_old <- xk
        xk <- xk_old - ff / dff
        ftest <- abs((xk - xk_old) / xk_old)
        if (ftest <= tol) {
          break
        }
      }
      ff <- c0 - g * xk * y
      out[f] <- xk
    }
  }
  out
}

#' Converts a 2D spectrum time series to a 1D spectrum
#'
#' @param spec a structure with the needed fields, as an output from 'get_2Dspectrum' for example
#' @param ... unused yet
#'
#' @return a structure comparable to 'get_1Dspectrum'.
#' @export
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `resourcecode` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("resourcecodedata", quietly = TRUE)) {
#' spec <- resourcecodedata::rscd_2d_spectra
#' spec1D_RSCD <- resourcecodedata::rscd_1d_spectra
#' spec1D <- convert_spectrum_2d1d(spec)
#' # Check the differences, should be low
#' max(abs(spec1D_RSCD$ef - spec1D$ef))
#'
#' # Plot the different spectrum
#' plot(spec1D$freq, spec1D$ef[, 1], type = "l", log = "xy")
#' lines(spec1D_RSCD$freq, spec1D_RSCD$ef[, 1], col = "red")
#'
#' # Images
#' lims <- c(0, 360)
#' r <- as.POSIXct(round(range(spec1D$forcings$time), "hours"))
#' oldpar <- par(mfcol = c(2, 1))
#' image(spec1D$forcings$time, spec1D$freq, t(spec1D$th1m),
#'   zlim = lims,
#'   xlab = "Time",
#'   ylab = "Freq (Hz)",
#'   xaxt = "n",
#'   main = "Directionnal spreading"
#' )
#' axis.POSIXct(1, spec1D$forcings$time,
#'   at = seq(r[1], r[2], by = "week"),
#'   format = "%Y-%m-%d",
#'   las = 2
#' )
#' image(spec1D_RSCD$forcings$time, spec1D_RSCD$freq, t(spec1D_RSCD$th1m),
#'   zlim = lims,
#'   xlab = "Time",
#'   ylab = "Freq (Hz)",
#'   xaxt = "n"
#' )
#' axis.POSIXct(1, spec1D$forcings$time,
#'   at = seq(r[1], r[2], by = "week"),
#'   format = "%Y-%m-%d",
#'   las = 2
#' )
#' par(oldpar)
#' }
convert_spectrum_2d1d <- function(spec, ...) {
  ddir <- diff(spec$dir)[1] * pi / 180 # computes the discretization in direction

  # Averages the directional spectrum along the direction
  # Simple sum is preferred to pracma::trapz for consistency with WWIII internals
  spec$ef <- apply(spec$efth * ddir, seq_along(dim(spec$efth))[-1], sum)

  # Computes the Mean directions and directionnal spreadings
  # (see 2.229 to 2.236 of WWIII user manual for the definitions

  # c1 = cos(theta)*F(theta,f)
  c1 <- sweep(spec$efth, 1, as.array(cos(spec$dir * pi / 180)), FUN = "*")
  # s1 = sin*F(theta,f)
  s1 <- sweep(spec$efth, 1, as.array(sin(spec$dir * pi / 180)), FUN = "*")
  # c2 = cos(2*theta)*F(theta,f)
  c2 <- sweep(spec$efth, 1, as.array(cos(2 * spec$dir * pi / 180)), FUN = "*")
  # s2 = sin(2*theta)*F(theta,f)
  s2 <- sweep(spec$efth, 1, as.array(sin(2 * spec$dir * pi / 180)), FUN = "*")

  # Same notation as the definitions above
  a1 <- apply(c1 * ddir, c(2, 3), sum)
  b1 <- apply(s1 * ddir, c(2, 3), sum)
  a2 <- apply(c2 * ddir, c(2, 3), sum)
  b2 <- apply(s2 * ddir, c(2, 3), sum)

  spec$th1m <- (atan2(b1, a1) * 180 / pi + 180) %% 360
  spec$th2m <- (atan2(b2, a2) * 180 / pi + 180) %% 360
  spec$sth1m <- sqrt(2 * (1 - sqrt((a1 ^ 2 + b1 ^ 2) / spec$ef ^ 2))) * 180 / pi
  spec$sth2m <- sqrt(2 * (1 - sqrt((a2 ^ 2 + b2 ^ 2) / spec$ef ^ 2))) * 180 / pi

  # Removes the 2D directional spectrum
  spec$efth <- NULL
  spec$dir <- NULL

  # In 1D spectra, the current direction is taken as "going to"
  spec$forcings$curdir <- (spec$forcings$curdir + 180) %% 360

  # reorder the elements to match the output from `get_1D_spectra`
  spec[c(1:4, 8:12, 5:7)]
}

#' Compute sea_state parameter from wave directional spectrum
#'
#' @param spec 2D spectrum data, e.g. from `get_2Dspectrum`
#' @param ... currently unused
#'
#' @return a tibble with the sea-state parameters computed from the time series of 2D spectrum
#' @export
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `resourcecode` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("resourcecodedata", quietly = TRUE)) {
#'  rscd_params <- get_parameters(
#'   node = "134865",
#'   start = "1994-01-01",
#'   end = "1994-01-31 23:00:00",
#'   parameters = c("hs", "tp", "cge", "t01", "dp", "dir")
#' )
#' spec <- resourcecodedata::rscd_2d_spectra
#' param_calc <- compute_sea_state_2d_spectrum(spec)
#' oldpar <- par(mfcol = c(2, 2))
#' plot(param_calc$time, param_calc$hs, type = "l", xlab = "Time", ylab = "Hs (m)")
#' lines(rscd_params$time, rscd_params$hs, col = "red")
#' plot(param_calc$time, param_calc$cge, type = "l", xlab = "Time", ylab = "CgE (kW/m)")
#' lines(rscd_params$time, rscd_params$cge, col = "red")
#' plot(param_calc$time, param_calc$tp, type = "l", xlab = "Time", ylab = "Tp (s)")
#' lines(rscd_params$time, rscd_params$tp, col = "red")
#' plot(param_calc$time, param_calc$dp, type = "l", xlab = "Time", ylab = "Dp (°)")
#' lines(rscd_params$time, rscd_params$dp, col = "red")
#' par(oldpar)
#' }
compute_sea_state_2d_spectrum <- function(spec, ...) {
  # Define an internal function that will do the job for a time-step
  # spectrum: 1D spectrum
  water_density <- 1026
  g <- 9.81

  # Compute 1d spectrum
  ddir <- diff(spec$dir)[1] * pi / 180
  spec_1d <- apply(spec$efth * ddir, c(2, 3), sum)

  # Compute spectral moments
  m0 <- apply(spec_1d, 2, pracma::trapz, x = spec$freq)
  m1 <- apply(sweep(spec_1d, 1, spec$freq, FUN = "*"),
              2,
              pracma::trapz,
              x = spec$freq)
  m2 <- apply(sweep(spec_1d, 1, spec$freq ^ 2, FUN = "*"),
              2,
              pracma::trapz,
              x = spec$freq)
  me <- apply(sweep(spec_1d, 1, 1 / spec$freq, FUN = "*"),
              2,
              pracma::trapz,
              x = spec$freq)

  out <- tibble::tibble(
    time = spec$forcings$time,
    hs = 4 * sqrt(m0),
    t01 = m0 / m1,
    t02 = sqrt(m0 / m2),
    te = me / m0
  )

  # fp evaluaton using spline fitting around spec_1d peak
  nk <- length(spec$freq)

  # Augment frequency resolution by 30
  freqp <- stats::approx(1:nk, spec$freq, xout = seq(
    from = 1,
    to = nk,
    length = 30 * nk
  ))
  spec_1d_smooth <- apply(spec_1d, 2, \(y) {
    stats::spline(x = spec$freq,
                  xout = freqp$y,
                  method = "natural",
                  y)$y
  }) # natural (i.e. "cubic") spline

  fp <- freqp$y[apply(spec_1d_smooth, 2, which.max)]
  out$tp <- 1 / fp

  # Get the forcings fields
  out$dpt <- spec$forcings$dpt
  out$wnd <- spec$forcings$wnd
  out$wnddir <- spec$forcings$wnddir
  out$cur <- spec$forcings$cur

  # In 1D spectral data, the current is "going to" cf user manual so we have to convert
  out$curdir <- (spec$forcings$curdir + 180) %% 360

  # Spectral Bandwidth and Peakedness parameter (Goda 1970)
  out$nu <- sqrt((m0 * m2) / (m1 ^ 2) - 1)
  out$mu <- sqrt(1 - m1 ^ 2 / (m0 * m2))

  # wave length
  disper_vec <- Vectorize(dispersion, vectorize.args = c("depth"))
  k <- disper_vec(spec$freq,
                  spec$forcings$dpt,
                  iter_max = 200,
                  tol = 1e-6)
  kd <- k * as.numeric(spec$forcings$dpt)

  out$km <- apply(k * spec_1d, 2, pracma::trapz, x = spec$freq) / m0
  out$lm <- 2 * pi / out$km

  # Group velocity
  c1 <- 1 + 2 * kd / sinh(2 * kd)
  c2 <- sqrt(g * tanh(kd) / k)
  cg <- 0.5 * c1 * c2


  # Energy flux
  out$cge <- water_density * g * apply(cg * spec_1d, 2, pracma::trapz, x = spec$freq) / 1000
  # convert to kW/m to be consistent with outputs from WWIII

  # Compute mean direction from  and spreading (°)
  aa <- sweep(spec$efth, 1, as.array(cos(spec$dir * pi / 180)), FUN = "*")
  bb <- sweep(spec$efth, 1, as.array(sin(spec$dir * pi / 180)), FUN = "*")

  am <- apply(apply(aa * ddir, c(2, 3), sum), 2, pracma::trapz, x = spec$freq)
  bm <- apply(apply(bb * ddir, c(2, 3), sum), 2, pracma::trapz, x = spec$freq)

  out$dir <- (atan2(bm, am) * 180 / pi + 180) %% 360
  out$spr <- (sqrt(2 * (1 - sqrt((
    am ^ 2 + bm ^ 2
  ) / m0 ^ 2))) * 180 / pi) %% 360

  # Compute mean direction and spreading at peak frequency (°)
  ind_efm <- apply(spec_1d, 2, which.max) # peak of the spectrum
  efm <- array(0, dim = c(36, dim(spec$efth)[3]))

  for (t in seq_len(dim(spec$efth)[3])) {
    efm[, t] <- spec$efth[, ind_efm[t], t]
  } # don't know how to avoid the loop, maybe using slice.index ?

  apm <- apply(sweep(efm, 1, as.array(cos(spec$dir * pi / 180)), "*") * ddir, 2, sum)
  bpm <- apply(sweep(efm, 1, as.array(sin(spec$dir * pi / 180)), "*") * ddir, 2, sum)

  out$dp <- (atan2(bpm, apm) * 180 / pi + 180) %% 360

  qpf <- apply(sweep(spec$efth ^ 2, 1, spec$freq, FUN = "*") * ddir, c(2, 3), sum)
  mq <- apply(qpf, 2, pracma::trapz, x = spec$freq)
  out$qp <- ((2 * mq / (m0 ^ 2)) * 180 / pi) %% 360

  out
}

#' Compute sea_state parameter from wave spectrum
#'
#' @param spec 1D spectrum data, e.g. from `get_1Dspectrum`
#' @param ... currently unused
#'
#' @return a tibble with the sea-state parameters computed from the time series of 2D spectrum
#' @export
#'
#' @examples
#' # Ensure that data package is available before running the example.
#' #  If it is not, see the `resourcecode` package vignette for details
#' # on installing the required data package.
#' if (requireNamespace("resourcecodedata", quietly = TRUE)) {
#' rscd_params <- get_parameters(
#'   node = "134865",
#'   start = "1994-01-01",
#'   end = "1994-01-31 23:00:00",
#'   parameters = c("hs", "tp", "cge", "t01", "dp", "dir")
#' )
#' spec <- resourcecodedata::rscd_1d_spectra
#' param_calc <- compute_sea_state_1d_spectrum(spec)
#' oldpar <- par(mfcol = c(2, 2))
#' plot(param_calc$time, param_calc$hs, type = "l", xlab = "Time", ylab = "Hs (m)")
#' lines(rscd_params$time, rscd_params$hs, col = "red")
#' plot(param_calc$time, param_calc$cge, type = "l", xlab = "Time", ylab = "CgE (kW/m)")
#' lines(rscd_params$time, rscd_params$cge, col = "red")
#' plot(param_calc$time, param_calc$tp, type = "l", xlab = "Time", ylab = "Tp (s)")
#' lines(rscd_params$time, rscd_params$tp, col = "red")
#' plot(param_calc$time, param_calc$dp, type = "l", xlab = "Time", ylab = "Peak direction (°)")
#' lines(rscd_params$time, rscd_params$dp, col = "red")
#' par(oldpar)
#' }
compute_sea_state_1d_spectrum <- function(spec, ...) {
  water_density <- 1026
  g <- 9.81

  # Compute spectral moments
  m0 <- apply(spec$ef, 2, pracma::trapz, x = spec$freq)
  m1 <- apply(sweep(spec$ef, 1, spec$freq, FUN = "*"),
              2,
              pracma::trapz,
              x = spec$freq)
  m2 <- apply(sweep(spec$ef, 1, spec$freq ^ 2, FUN = "*"),
              2,
              pracma::trapz,
              x = spec$freq)
  me <- apply(sweep(spec$ef, 1, 1 / spec$freq, FUN = "*"),
              2,
              pracma::trapz,
              x = spec$freq)

  out <- tibble::tibble(
    time = spec$forcings$time,
    hs = 4 * sqrt(m0),
    t01 = m0 / m1,
    t02 = sqrt(m0 / m2),
    te = me / m0
  )

  # fp evaluaton using spline fitting around Ef peak
  nk <- length(spec$freq)
  # Augment frequency resolution by 30
  freqp <- stats::approx(1:nk, spec$freq, xout = seq(
    from = 1,
    to = nk,
    length = 30 * nk
  ))
  spec_1d_smooth <- apply(spec$ef, 2, \(y) {
    stats::spline(x = spec$freq,
                  xout = freqp$y,
                  method = "natural",
                  y)$y
  }) # natural (i.e. "cubic") spline

  fp <- freqp$y[apply(spec_1d_smooth, 2, which.max)]
  out$tp <- 1 / fp

  # Get the forcings fields
  out$dpt <- spec$forcings$dpt
  out$wnd <- spec$forcings$wnd
  out$wnddir <- spec$forcings$wnddir
  out$cur <- spec$forcings$cur
  out$curdir <- spec$forcings$curdir

  # Spectral Bandwidth and Peakedness parameter (Goda 1970)
  out$nu <- sqrt((m0 * m2) / (m1 ^ 2) - 1)
  out$mu <- sqrt(1 - m1 ^ 2 / (m0 * m2))

  # wave length
  disper_vec <- Vectorize(dispersion, vectorize.args = c("depth"))
  k <- disper_vec(spec$freq,
                  spec$forcings$dpt,
                  iter_max = 200,
                  tol = 1e-6)
  kd <- k * as.numeric(spec$forcings$dpt)

  out$km <- apply(k * spec$ef, 2, pracma::trapz, x = spec$freq) / m0
  out$lm <- 2 * pi / out$km

  # Group velocity
  c1 <- 1 + 2 * kd / sinh(2 * kd)
  c2 <- sqrt(g * tanh(kd) / k)
  cg <- 0.5 * c1 * c2

  # Energy flux, converted to kW/m to be consistent with outputs from WWIII
  out$cge <- water_density * g * apply(cg * spec$ef, 2, pracma::trapz, x = spec$freq) / 1000

  # Compute mean direction from  and spreading (°)

  out$dir <- apply(spec$th1m, 2, pracma::trapz, x = spec$freq) # possibly wrong
  out$spr <- apply(spec$sth1m, 2, pracma::trapz, x = spec$freq) # possibly wrong

  # mean direction at peak frequency (°)
  ind_efm <- apply(spec$ef, 2, which.max) # peak of the spectrum
  dp <- vector("numeric", dim(spec$ef)[2])

  for (t in seq_len(dim(spec$ef)[2])) {
    dp[t] <- spec$th1m[ind_efm[t], t]
  } # dont know how to avoid the loop, maybe using slice.index?
  out$dp <- dp

  out
}
