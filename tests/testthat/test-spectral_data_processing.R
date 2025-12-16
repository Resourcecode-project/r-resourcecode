test_that("dispersion is computed correctly", {
  true_dispersion <- c(
    0.000000000,
    0.003669946,
    0.007384602,
    0.011190670,
    0.015138944,
    0.019286586,
    0.023699621,
    0.028455513,
    0.033645447,
    0.039375373,
    0.045764159,
    0.052936566,
    0.061009373,
    0.070072391,
    0.080171849,
    0.091306267,
    0.103438532,
    0.116516596,
    0.130491221,
    0.145324833,
    0.160992686,
    0.177480208,
    0.194779565,
    0.212886864,
    0.231800306,
    0.251519111,
    0.272042963,
    0.293371740,
    0.315505400,
    0.338443928,
    0.362187318,
    0.386735569,
    0.412088681,
    0.438246654,
    0.465209488,
    0.492977182,
    0.521549737,
    0.550927153,
    0.581109429,
    0.612096567,
    0.643888564,
    0.676485423,
    0.709887142,
    0.744093722,
    0.779105163,
    0.814921464,
    0.851542626,
    0.888968649,
    0.927199533,
    0.966235277,
    1.006075882
  )
  expect_equal(dispersion(0, Inf), 0)
  expect_equal(
    dispersion(1 / 100 * (0:50), depth = 30),
    true_dispersion
  )
})

test_that("Conversion between 2D and 1D spectra works", {
  spec <- resourcecodedata::rscd_2d_spectra
  spec1d_rscd <- resourcecodedata::rscd_1d_spectra
  spec_1d <- convert_spectrum_2d1d(spec)
  expect_equal(names(spec_1d), names(spec1d_rscd))
  expect_equal(spec_1d$ef, spec1d_rscd$ef, tolerance = 1e-7)
  expect_equal(spec_1d$forcings, spec1d_rscd$forcings[, 1:6], tolerance = 1e-7)
})


test_that("Computation of sea-state parameters works", {
  spec <- resourcecodedata::rscd_2d_spectra
  spec1d_rscd <- resourcecodedata::rscd_1d_spectra

  spec_1d <- convert_spectrum_2d1d(spec)
  sea_state_from2d <- compute_sea_state_2d_spectrum(spec)
  sea_state_from1d <- compute_sea_state_1d_spectrum(spec1d_rscd)
  sea_state_from1d_computed <- compute_sea_state_1d_spectrum(spec_1d)
  expect_equal(
    sea_state_from1d[, 1:16],
    sea_state_from2d[, 1:16],
    tolerance = 1e-7
  )
  expect_equal(
    sea_state_from1d_computed[, 1:16],
    sea_state_from1d[, 1:16],
    tolerance = 1e-7
  )
})
