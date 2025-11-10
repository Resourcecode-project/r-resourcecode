# Mean Direction

Function for computing the (weighted) arithmetic mean of directional
data in **meteorological convention**.

## Usage

``` r
mean_direction(directions, weights = NULL)
```

## Arguments

- directions:

  numeric vector of directions, in degree, 0° being the North

- weights:

  numeric vector, usually wind speed of wave height.

## Value

The (weighted) mean of the values in `directions` is computed.

## Examples

``` r
# Test with some wind directions (unweighted)
wind_directions <- c(10, 20, 350, 5, 15) # Directions mostly around North
mean_dir <- mean_direction(wind_directions)
cat("Mean wind direction (unweighted):", round(mean_dir, 1), "degrees\n")
#> Mean wind direction (unweighted): 8 degrees
wind_directions <- c(350, 10, 20, 340, 30) # Directions around North
wind_speeds <- c(15, 5, 2, 12, 3) # Higher speeds for directions closer to North
mean_dir_weighted <- mean_direction(wind_directions, wind_speeds)
cat("Mean wind direction (weighted):", round(mean_dir_weighted, 1), "degrees\n")
#> Mean wind direction (weighted): 354.1 degrees

# Compare weighted vs unweighted for the same data
mean_dir_unweighted <- mean_direction(wind_directions)
cat("Same data unweighted:", round(mean_dir_unweighted, 1), "degrees\n")
#> Same data unweighted: 6.1 degrees
```
