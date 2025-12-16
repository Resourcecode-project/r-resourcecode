# Find the closest point of the FIELD grid to the specified position

Find the closest point of the FIELD grid to the specified position

## Usage

``` r
closest_point_field(x, lat = NULL, closest = 1L, ...)
```

## Arguments

- x:

  vector of coordinates in the form longitude/latitude data frame

- lat:

  alternatively, x and lat can be vector of the same length

- closest:

  an integer to specify the number of point to output.

- ...:

  currently unused

## Value

a list with two components: the closest point(s) of the grid and the
distance (s).

## Examples

``` r
semrev_west <- closest_point_field(c(-2.786, 47.239))
semrev_west
#> $points
#>        [,1]
#> [1,] 120155
#> 
#> $distances
#>          [,1]
#> [1,] 288.0243
#> 
```
