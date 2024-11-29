test_that("we can plot the 2D spectrum", {
  skip_if_offline()
  spec <- get_2d_spectrum("SEMREVO", start = "1994-01-01", end = "1994-01-01")
  plot_from_index <- plot_2d_specta(spec, 1)
  plot_not_normalized <- plot_2d_specta(spec, 1, normalize = FALSE)
  plot_from_date <- plot_2d_specta(spec, "1994-01-27 13:00:00")
  expect_warning(print(plot_from_index))
  expect_warning(print(plot_not_normalized))
  expect_warning(print(plot_from_date))
  suppressWarnings({
    vdiffr::expect_doppelganger("Specifying index", plot_from_index)
    vdiffr::expect_doppelganger("Un-normalized 2D spectra", plot_not_normalized)
    vdiffr::expect_doppelganger("Plot by date", plot_from_date)
  })
})

test_that("Plotting maps works", {
  vdiffr::expect_doppelganger("Mapping water depth",
                              rscd_mapplot(resourcecodedata::rscd_field$depth, name = "Depth (m)"))
})
