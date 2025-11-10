# Get season from date time object

Get season from date time object

## Usage

``` r
cut_seasons(
  datetime,
  definition = "meteorological",
  hemisphere = "northern",
  labels = NULL
)
```

## Arguments

- datetime:

  a POSIXct vector from with the season is constructed

- definition:

  the definition used to compute the season. See details section.

- hemisphere:

  in the Southern hemisphere, seasons are reversed compared to the
  Northern one.

- labels:

  optional, a character vector of length fours with the seasons' names.

## Value

a Factor vector with 4 levels depending on the definitions (and labels
if provided)

## Details

Available Definitions:

- meteorological: Standard seasons (Dec-Feb = Winter, etc.)

- astronomical: Based on equinoxes/solstices

- djf: Dec-Jan-Feb, Mar-Apr-May, Jun-Jul-Aug, Sep-Oct-Nov

- jfm: Jan-Feb-Mar, Apr-May-Jun, Jul-Aug-Sep, Oct-Nov-Dec

- fma: Feb-Mar-Apr, May-Jun-Jul, Aug-Sep-Oct, Nov-Dec-Jan

- amj, jas, ond: Alternative starting points for quarterly seasons

## Examples

``` r
dates <- seq(
  from = as.POSIXct("2023-01-15"),
  to = as.POSIXct("2023-12-15"),
  by = "month"
)
cut_seasons(dates)
#>  [1] Winter Winter Spring Spring Spring Summer Summer Summer Autumn Autumn
#> [11] Autumn Winter
#> Levels: Spring Summer Autumn Winter
```
