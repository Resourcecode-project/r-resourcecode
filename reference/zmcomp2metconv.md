# Convert u/v to meteorological wind speed and direction

Converts wind or current zonal and meridional velocity components to
magnitude and direction according to meteorological convention.

## Usage

``` r
zmcomp2metconv(u, v = NULL, names = c("wspd", "wdir"))
```

## Arguments

- u:

  zonal velocity (1D vector) or matrix with zonal and meridional
  velocity (Nx2 matrix)

- v:

  meridional velocity (1D vector)

- names:

  names to construct the resulting data.frame

## Value

a Nx2 data.frame with the norm and direction (meteorological convention)

## Examples

``` r
u <- matrix(rnorm(200), nrow = 100, ncol = 2)
vdir <- zmcomp2metconv(u)
```
