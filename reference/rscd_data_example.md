# Resourcecode data extract

An extract of the Resourcecode Hindcast database, at node 123456,
spanning from 1994-01-01 00:00:00 to 1999-12-31 23:00:00.

## Usage

``` r
rscd_data_example
```

## Format

### A data frame with 87,648 rows and 7 variables:

- time:

  POSIXct. Timestamp in UTC (hourly resolution).

- hs:

  numeric. Significant wave height (m).

- tp:

  numeric. Peak wave period (s).

- dp:

  numeric. Mean wave direction (degrees, coming from true North,
  clockwise).

- uwnd:

  numeric. Zonal (east–west) component of the 10-m wind (m/s). Positive
  eastward.

- vwnd:

  numeric. Meridional (north–south) component of the 10-m wind (m/s).
  Positive northward.

- dpt:

  numeric.Depth (m).

## Source

User Manual of the RESOURCECODE database
<https://archimer.ifremer.fr/doc/00751/86306/>
