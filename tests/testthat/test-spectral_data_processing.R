test_that("dispersion is computed correctly", {
  expect_equal(dispersion(0, Inf), 0)
  expect_snapshot_value(dispersion(1 / 100 * (0:50), depth = 30), style = "serialize")
})

test_that("Conversion between 2D and 1D spectra works", {
  skip_if_offline()
  skip_if(!requireNamespace("resourcecodedata", quietly = TRUE))
  spec <- get_2d_spectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
  spec1d_rscd <- get_1d_spectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
  spec_1d <- convert_spectrum_2d1d(spec)
  expect_equal(names(spec_1d), names(spec1d_rscd))
  expect_equal(spec_1d$ef, spec1d_rscd$ef, tolerance = 1e-7)
  expect_equal(spec_1d$forcings, spec1d_rscd$forcings[, 1:6], tolerance = 1e-7)
})


test_that("Computation of sea-state parameters works", {
  skip_if_offline()
  skip_if(!requireNamespace("resourcecodedata", quietly = TRUE))
  spec <- get_2d_spectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
  spec1d_rscd <- get_1d_spectrum("SEMREVO", start = "1994-01-01", end = "1994-02-28")
  spec_1d <- convert_spectrum_2d1d(spec)
  sea_state_from2d <- compute_sea_state_2d_spectrum(spec)
  sea_state_from1d <- compute_sea_state_1d_spectrum(spec1d_rscd)
  sea_state_from1d_computed <- compute_sea_state_1d_spectrum(spec_1d)
  expect_equal(sea_state_from1d[, 1:16], sea_state_from2d[, 1:16], tolerance = 1e-7)
  expect_equal(sea_state_from1d_computed[, 1:16], sea_state_from1d[, 1:16], tolerance = 1e-7)
})
