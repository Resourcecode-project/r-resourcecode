# Directional binning

Cuts direction vector into directional bins

## Usage

``` r
cut_directions(directions, n_bins = 8, labels = NULL)
```

## Arguments

- directions:

  vector of directions to be binned, in degree, 0° being the North.

- n_bins:

  number of bins, default: 8 sectors.

- labels:

  optional character vector giving the sectors names.

## Value

a factor vector the same size as `directions` with the values binned
into sectors.

## Examples

``` r
# Example usage and demonstration
set.seed(123)
directions <- runif(20, 0, 360)

# Test with different numbers of bins
cat("Original directions:\n")
#> Original directions:
print(round(directions, 1))
#>  [1] 103.5 283.8 147.2 317.9 338.6  16.4 190.1 321.3 198.5 164.4 344.5 163.2
#> [13] 243.9 206.1  37.1 323.9  88.6  15.1 118.1 343.6

cat("\n8 bins (default):\n")
#> 
#> 8 bins (default):
bins_8 <- cut_directions(directions, n_bins = 8)
print(bins_8)
#>  [1] E  W  SE NW N  N  S  NW S  S  N  S  SW SW NE NW E  N  SE N 
#> Levels: N NE E SE S SW W NW

cat("\n4 bins:\n")
#> 
#> 4 bins:
bins_4 <- cut_directions(directions, n_bins = 4)
print(bins_4)
#>  [1] E W S N N N S N S S N S W S N N E N E N
#> Levels: N E S W
```
