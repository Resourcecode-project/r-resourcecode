# resourcecode (development version)

- `closest_point_field()` and `closest_point_field()` now accept a list of locations
  instead of a single one (#3).

# resourcecode 0.3.0

- `rscd_mapplot()`: add Resourcecode caption.
- New helpers:
  - `mean_direction()` to compute (weighted) mean values of directional variables,
  - `cut_directions()` to bin a vector of directions into sectors, centered North;
  - `cut_seasons()` to compute the season from a datetime, with different definitions.
 - Fixed #7 (thanks @paalves).

# resourcecode 0.2.1

- Move out the data from the package to `{resourcecodedata}` package to be compliant with
CRAN limiting size of 5Mb.

# resourcecode 0.1.0

- Add some code linter;
- Switch from `ftp`to `https` to download the spectral data, to increase stability;
- Fix typos in the documentation (thanks to @jlegrand35);
- `zmcomp2metconv` outputs a data.frame and gains an argument to name columns;
- Fixed deprecated arguments in `ggplot::theme()` used in `rscd_mapplot`;
- rename`compute_orbital_speeds` to `compute_orbital_speeds`;
- remove dependency to `{arrow}`;
- make data smaller by using `tools::resaveRdaFiles("data/")`.

# resourcecode 0.0.1

Initial version with very crude functionalities

- Database configuration: coastline, depth, output variables, grids...
- Download sea-state parameters of the FIELD grid from Casandra database;
- Download 1D and 2D spectral data from the netCDF files via FTP;
- Helpers to plot maps (e.g. bathymetry or spatial statistics);
- Plot 1D and 2 spectrum;
- Compute sea-state from 1D and 2D spectrum.
