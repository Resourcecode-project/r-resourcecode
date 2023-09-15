test_that("we can plot the 2D spectrum", {
  skip_if_offline()
  spec = get_2Dspectrum("SEMREVO",start="1994-01-01",end="1994-01-01")
  expect_snapshot_value(plot_2Dspecta(spec,1),style="serialize")
  expect_snapshot_value(plot_2Dspecta(spec,1,normalize = FALSE),style="serialize")
  expect_snapshot_value(plot_2Dspecta(spec,"1994-01-27 13:00:00"),style="serialize")
})
