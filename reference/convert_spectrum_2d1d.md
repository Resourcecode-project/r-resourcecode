# Converts a 2D spectrum time series to a 1D spectrum

Converts a 2D spectrum time series to a 1D spectrum

## Usage

``` r
convert_spectrum_2d1d(spec, ...)
```

## Arguments

- spec:

  a structure with the needed fields, as an output from 'get_2Dspectrum'
  for example

- ...:

  unused yet

## Value

a structure comparable to 'get_1Dspectrum'.

## Examples

``` r
spec <- resourcecodedata::rscd_2d_spectra
spec1D_RSCD <- resourcecodedata::rscd_1d_spectra
spec1D <- convert_spectrum_2d1d(spec)
# Check the differences, should be low
max(abs(spec1D_RSCD$ef - spec1D$ef))
#> [1] 1.101815e-05

# Plot the different spectrum
plot(spec1D$freq, spec1D$ef[, 1], type = "l", log = "xy")
lines(spec1D_RSCD$freq, spec1D_RSCD$ef[, 1], col = "red")


# Images
lims <- c(0, 360)
r <- as.POSIXct(round(range(spec1D$forcings$time), "hours"))
oldpar <- par(mfcol = c(2, 1))
image(spec1D$forcings$time, spec1D$freq, t(spec1D$th1m),
  zlim = lims,
  xlab = "Time",
  ylab = "Freq (Hz)",
  xaxt = "n",
  main = "Directionnal spreading"
)
axis.POSIXct(1, spec1D$forcings$time,
  at = seq(r[1], r[2], by = "week"),
  format = "%Y-%m-%d",
  las = 2
)
image(spec1D_RSCD$forcings$time, spec1D_RSCD$freq, t(spec1D_RSCD$th1m),
  zlim = lims,
  xlab = "Time",
  ylab = "Freq (Hz)",
  xaxt = "n"
)
axis.POSIXct(1, spec1D$forcings$time,
  at = seq(r[1], r[2], by = "week"),
  format = "%Y-%m-%d",
  las = 2
)

par(oldpar)
```
