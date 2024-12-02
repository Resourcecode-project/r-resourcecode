test_that("we can plot the 2D spectrum", {
  skip_if_offline()
  skip_if(!requireNamespace("resourcecodedata", quietly = TRUE))
  spec <- resourcecodedata::rscd_2d_spectra
  plot_from_index <- plot_2d_specta(spec, 1)
  plot_not_normalized <- plot_2d_specta(spec, 1, normalize = FALSE)
  plot_from_date <- plot_2d_specta(spec, "1994-01-27 13:00:00")
  expect_warning(print(plot_from_index))
  expect_warning(print(plot_not_normalized))
  expect_warning(print(plot_from_date))
  suppressWarnings({
    vdiffr::expect_doppelganger("Specifying index",
      plot_from_index,
      variant = Sys.info()[["sysname"]]
    )
    vdiffr::expect_doppelganger("Un-normalized 2D spectra",
      plot_not_normalized,
      variant = Sys.info()[["sysname"]]
    )
    vdiffr::expect_doppelganger("Plot by date",
      plot_from_date,
      variant = Sys.info()[["sysname"]]
    )
  })
})

test_that("Plotting maps works", {
  skip_if(!requireNamespace("resourcecodedata", quietly = TRUE))
  vdiffr::expect_doppelganger(
    "Mapping water depth",
    rscd_mapplot(resourcecodedata::rscd_field$depth, name = "Depth (m)"),
    variant = Sys.info()[["sysname"]]
  )
})
