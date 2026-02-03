# Download time series of sea-state parameters from RESOURCECODE database

If the remote resource is unavailable or returns an error, the function
returns NULL and emits an informative message.

## Usage

``` r
get_parameters(
  parameters = "hs",
  node = 42,
  start = as.POSIXct("1994-01-01 00:00:00", tz = "UTC"),
  end = as.POSIXct("1994-12-31 23:00:00", tz = "UTC")
)
```

## Arguments

- parameters:

  character vector of sea-state parameters

- node:

  single integer with the node to get

- start:

  starting date (as integer, character or posixct)

- end:

  ending date (as integer, character or posixct)

## Value

a tibble with N-rows and `length(parameters)` columns.

## Examples

``` r
rscd_data <- get_parameters(parameters = c("hs", "tp"), node = 42)
#> Network error: Could not connect to the remote resource. The server may be unavailable.
#> Failed to retrieve parameter: hs
if(!is.null(rscd_data)) plot(rscd_data$time, rscd_data$hs, type = "l")
```
