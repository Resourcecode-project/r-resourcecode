test_that("dispersion is computed correctly", {
  expect_equal(dispersion(0, Inf), 0)
  expect_snapshot_value(dispersion(1 / 100 * (0:50), depth = 30), style = "serialize")
})

test_that("Conversion between 2D and 1D spectra works", {
  spec <- get_2Dspectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
  spec1D_RSCD <- get_1Dspectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
  spec1D <- convert_spectrum_2D1D(spec)
  expect_equal(names(spec1D), names(spec1D_RSCD))
  expect_equal(spec1D$ef, spec1D_RSCD$ef, tolerance = 1e-7)
  expect_equal(spec1D$forcings, spec1D_RSCD$forcings[, 1:6], tolerance = 1e-7)
})


test_that("Computation of sea-state parameters works", {
  spec <- get_2Dspectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
  spec1D_RSCD <- get_1Dspectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
  spec1D <- convert_spectrum_2D1D(spec)
  sea_state_from2D <- compute_sea_state_2Dspectrum(spec)
  sea_state_from1D <- compute_sea_state_1Dspectrum(spec1D_RSCD)
  sea_state_from1D_computed <- compute_sea_state_1Dspectrum(spec1D)
  expect_equal(sea_state_from1D[, 1:16], sea_state_from2D[, 1:16], tolerance = 1e-7)
  expect_equal(sea_state_from1D_computed[, 1:16], sea_state_from1D[, 1:16], tolerance = 1e-7)
})
