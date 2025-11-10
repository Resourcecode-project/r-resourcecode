# Compute Weather Windows

Computes and returns start date of each weather window, implemented in
C++ for speed.

## Usage

``` r
weather_windows(
  valid_periods,
  window_length,
  allow_overlap = TRUE,
  time_step = 3600
)
```

## Arguments

- valid_periods:

  A data frame with a 'time' column (POSIXct).

- window_length:

  Minimum window duration (hours).

- allow_overlap:

  Logical; If TRUE, the algorithm searches for window, if a window is
  found, search of next window will start from the end of the previous
  window. If FALSE, it uses continuous window search: The algorithm
  searches for window starting from every time step that meets the
  criteria.

- time_step:

  Expected time step between consecutive timestamps (seconds).

## Value

POSIXct vector of detected window start times.
