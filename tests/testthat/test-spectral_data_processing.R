test_that("dispersion is computed correctly",{
  expect_equal(dispersion(0,Inf),0)
  expect_snapshot_value(dispersion(1/100*(0:50),depth = 30))
})

test_path("Conversion between 2D and 1D spectra works",{
  spec = get_2Dspectrum("SEMREVO",start="1994-01-01",end="1994-02-28")
  spec1D_RSCD = get_1Dspectrum("SEMREVO",start="1994-01-01",end="1994-02-28")
  spec1D = convert_spectrum_2D1D(spec)
  expect_equal(names(spec1D), names(spec1D_RSCD))
  expect_equal(spec1D$ef,spec1D_RSCD$ef,tolerance = 1e-5)
  expect_equal(NROW(spec1D$forcings),NROW(spec1D_RSCD$forcings))
})
