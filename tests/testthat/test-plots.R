test_that("we can plot the 2D spectrum", {
  skip_if_offline()
  spec = get_2Dspectrum("SEMREVO",start="1994-01-01",end="1994-01-01")
  vdiffr::expect_doppelganger("Specifying index",plot_2Dspecta(spec,1))
  vdiffr::expect_doppelganger("Un-normalized 2D spectra",plot_2Dspecta(spec,1,normalize = FALSE))
  vdiffr::expect_doppelganger("Plot by date",plot_2Dspecta(spec,"1994-01-27 13:00:00"))
})
