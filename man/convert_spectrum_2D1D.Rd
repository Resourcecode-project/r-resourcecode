% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specta_data_processing.R
\name{convert_spectrum_2D1D}
\alias{convert_spectrum_2D1D}
\title{Converts a 2D spectrum time series to a 1D spectrum}
\usage{
convert_spectrum_2D1D(spec, ...)
}
\arguments{
\item{spec}{a structure with the needed fields, as an output from 'get_2Dspectrum' for example}

\item{...}{unused yet}
}
\value{
a structure comparable to 'get_1Dspectrum'.
}
\description{
Converts a 2D spectrum time series to a 1D spectrum
}
\examples{
spec <- get_2Dspectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
spec1D_RSCD <- get_1Dspectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
spec1D <- convert_spectrum_2D1D(spec)
# Check the differences, should be low
max(abs(spec1D_RSCD$ef - spec1D$ef))

# Plot the different spectrum
plot(spec1D$freq, spec1D$ef[, 1], type = "l", log = "xy")
lines(spec1D_RSCD$freq, spec1D_RSCD$ef[, 1], col = "red")

# Images
lims <- c(0, 360)
r <- as.POSIXct(round(range(spec1D$forcings$time), "hours"))
par(mfcol = c(2, 1))
image(spec1D$forcings$time, spec1D$freq, t(spec1D$th1m),
  zlim = lims,
  xlab = "Time",
  ylab = "Freq (Hz)",
  xaxt = "n",
  main = "Directionnal spreading"
)
axis.POSIXct(1, spec1D$forcings$time,
 at = seq(r[1], r[2], by = "week"),
 format = "\%Y-\%m-\%d",
 las = 2)
image(spec1D_RSCD$forcings$time, spec1D_RSCD$freq, t(spec1D_RSCD$th1m),
  zlim = lims,
  xlab = "Time",
  ylab = "Freq (Hz)",
  xaxt = "n"
)
axis.POSIXct(1, spec1D$forcings$time,
at = seq(r[1], r[2], by = "week"),
format = "\%Y-\%m-\%d",
las = 2)
}
