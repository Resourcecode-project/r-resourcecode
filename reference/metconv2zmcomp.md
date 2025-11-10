# Convert meteorological wind speed and direction to u/v components

Converts wind speed (magnitude) and direction (in degrees,
meteorological convention: direction from which the wind blows, measured
clockwise from north) into zonal (u) and meridional (v) components.

## Usage

``` r
metconv2zmcomp(speed, direction, names = c("uwnd", "vwnd"))
```

## Arguments

- speed:

  Numeric vector of wind speeds.

- direction:

  Numeric vector of wind directions in degrees (0° = from north, 90° =
  from east, 180° = from south, 270° = from west).

- names:

  (optional) ames to construct the resulting data.frame.

## Value

A data.frame with two columns:

- u:

  Zonal wind component (m/s), positive eastward.

- v:

  Meridional wind component (m/s), positive northward.

## Examples

``` r
# Example 1: North wind of 10 m/s (blowing southward)
metconv2zmcomp(10, 0)
#>   uwnd vwnd
#> 1    0  -10

# Example 2: East wind of 5 m/s (blowing westward)
metconv2zmcomp(5, 90)
#>   uwnd          vwnd
#> 1   -5 -3.061617e-16

# Example 3: South wind of 8 m/s (blowing northward)
metconv2zmcomp(8, 180)
#>            uwnd vwnd
#> 1 -9.797174e-16    8
```
