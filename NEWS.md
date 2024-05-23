# `{resourcecode}` (development version)

- Typos in the documentation (@jlegrand35)
- `zmcomp2metconv` outputs a data.frame and gains an argument to name columns
- Fixed deprecated arguments in `ggplot::theme()` used in `rscd_mapplot`
- rename`compute_orbital_speeds` to `compute_orbital_speeds`
- remove dependency to `{arrow}`
- make data smaller by using `tools::resaveRdaFiles("data/")`

## Initial version with very crude functionalities

- Database configuration: coastline, depth, output variables, grids...
- Download sea-state parameters of the FIELD grid from Casandra database;
- Download 1D and 2D spectral data from the netCDF files via FTP
- Helpers to plot maps (e.g. bathymetry or spatial statistics)
- Plot 1D and 2 spectrum
- Compute sea-state from 1D and 2D spectrum
