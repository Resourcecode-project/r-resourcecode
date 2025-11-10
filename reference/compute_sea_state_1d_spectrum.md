# Compute sea_state parameter from wave spectrum

Compute sea_state parameter from wave spectrum

## Usage

``` r
compute_sea_state_1d_spectrum(spec, ...)
```

## Arguments

- spec:

  1D spectrum data, e.g. from `get_1Dspectrum`

- ...:

  currently unused

## Value

a tibble with the sea-state parameters computed from the time series of
2D spectrum

## Examples

``` r
rscd_params <- get_parameters(
  node = "134865",
  start = "1994-01-01",
  end = "1994-01-31 23:00:00",
  parameters = c("hs", "tp", "cge", "t01", "dp", "dir")
)
spec <- resourcecodedata::rscd_1d_spectra
param_calc <- compute_sea_state_1d_spectrum(spec)
oldpar <- par(mfcol = c(2, 2))
plot(param_calc$time, param_calc$hs, type = "l", xlab = "Time", ylab = "Hs (m)")
lines(rscd_params$time, rscd_params$hs, col = "red")
plot(param_calc$time, param_calc$cge, type = "l", xlab = "Time", ylab = "CgE (kW/m)")
lines(rscd_params$time, rscd_params$cge, col = "red")
plot(param_calc$time, param_calc$tp, type = "l", xlab = "Time", ylab = "Tp (s)")
lines(rscd_params$time, rscd_params$tp, col = "red")
plot(param_calc$time, param_calc$dp, type = "l", xlab = "Time", ylab = "Peak direction (°)")
lines(rscd_params$time, rscd_params$dp, col = "red")

par(oldpar)
```
