# Changelog

## resourcecode 0.5.1

- Remove dependency on [latex2exp](https://www.stefanom.io/latex2exp/)
  that is scheduled for archival on 2026-01-03

## resourcecode 0.5.0

CRAN release: 2025-11-10

- Update the data retrieving function to fail gracefully whenever the
  remote database is not accessible.
- New function ‘weather_windows()’ to compute weather windows of a given
  duration.
- Update the ending date of the Hindcast to 2024-12-31.

## resourcecode 0.4.0

CRAN release: 2025-10-22

- New example data: `rscd_data_example` with an extract of some
  variables at a given node. See
  [`?rscd_data_example`](https://resourcecode-project.github.io/r-resourcecode/reference/rscd_data_example.md)
  for details.
- New helper:
  [`fractional_day_of_year()`](https://resourcecode-project.github.io/r-resourcecode/reference/fractional_day_of_year.md)
  to compute the fractional time since the beginning of a year.
- New function
  [`plot_1d_specta()`](https://resourcecode-project.github.io/r-resourcecode/reference/plot_1d_specta.md)
  to plot 1D spectral data, with sea-states parameters.
- New function
  [`metconv2zmcomp()`](https://resourcecode-project.github.io/r-resourcecode/reference/metconv2zmcomp.md)
  to compute the u/v components. from meteorological wind speed and
  direction.
- Adds
  [resourcecodedata](https://github.com/Resourcecode-project/r-resourcecodedata/)
  package as dependence, to ease the use of the package .
- [`closest_point_field()`](https://resourcecode-project.github.io/r-resourcecode/reference/closest_point_field.md)
  and
  [`closest_point_field()`](https://resourcecode-project.github.io/r-resourcecode/reference/closest_point_field.md)
  now accept a list of locations instead of a single one
  ([\#3](https://github.com/Resourcecode-project/r-resourcecode/issues/3)).

## resourcecode 0.3.0

CRAN release: 2025-08-21

- [`rscd_mapplot()`](https://resourcecode-project.github.io/r-resourcecode/reference/rscd_mapplot.md):
  add Resourcecode caption.
- New helpers:
  - [`mean_direction()`](https://resourcecode-project.github.io/r-resourcecode/reference/mean_direction.md)
    to compute (weighted) mean values of directional variables,
  - [`cut_directions()`](https://resourcecode-project.github.io/r-resourcecode/reference/cut_directions.md)
    to bin a vector of directions into sectors, centered North;
  - [`cut_seasons()`](https://resourcecode-project.github.io/r-resourcecode/reference/cut_seasons.md)
    to compute the season from a datetime, with different definitions.
- Fixed
  [\#7](https://github.com/Resourcecode-project/r-resourcecode/issues/7)
  (thanks [@paalves](https://github.com/paalves)).

## resourcecode 0.2.1

CRAN release: 2024-12-03

- Move out the data from the package to
  [resourcecodedata](https://github.com/Resourcecode-project/r-resourcecodedata/)
  package to be compliant with CRAN limiting size of 5Mb.

## resourcecode 0.1.0

- Add some code linter;
- Switch from `ftp`to `https` to download the spectral data, to increase
  stability;
- Fix typos in the documentation (thanks to
  [@jlegrand35](https://github.com/jlegrand35));
- `zmcomp2metconv` outputs a data.frame and gains an argument to name
  columns;
- Fixed deprecated arguments in `ggplot::theme()` used in
  `rscd_mapplot`;
- rename`compute_orbital_speeds` to `compute_orbital_speeds`;
- remove dependency to [arrow](https://github.com/apache/arrow/);
- make data smaller by using `tools::resaveRdaFiles("data/")`.

## resourcecode 0.0.1

Initial version with very crude functionalities

- Database configuration: coastline, depth, output variables, grids…
- Download sea-state parameters of the FIELD grid from Casandra
  database;
- Download 1D and 2D spectral data from the netCDF files via FTP;
- Helpers to plot maps (e.g. bathymetry or spatial statistics);
- Plot 1D and 2 spectrum;
- Compute sea-state from 1D and 2D spectrum.
