
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Resource**code** <a href="https://github.com/Resourcecode-project/r-resourcecode"><img src="man/figures/logo.png" align="right" height="139" alt="resourcecode website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Resourcecode-project/r-resourcecode/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/Resourcecode-project/r-resourcecode/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/NRaillard/resourcecode/branch/main/graph/badge.svg)](https://app.codecov.io/gh/NRaillard/resourcecode?branch=main)
[![resourcecode status
badge](https://resourcecode-project.r-universe.dev/resourcecode/badges/version)](https://resourcecode-project.r-universe.dev/resourcecode)
<!-- badges: end -->

The goal of `{resourcecode}` is to provide an easy access to the
Resourcecode hindcast database of sea-states. More information on the
database can be found [here](https://resourcecode.ifremer.fr). This
website only contains information about the way to retrieve data from
this data base and the functionalities offred by this package.

## Installation

This package depends on data in a data package `{resourcecodedata}` that
is available through a `drat` repository on GitHub. To use the
`{resourcecode}` package, you will need to install `{resourcecodedata}`
on your computer, using the following `install.packages` function (and
later update it using the `update.packages` function):

``` r
install.packages("resourcecodedata", repos = c("https://resourcecode-project.r-universe.dev", "https://cloud.r-project.org"))
```

The `{resourcecode}` package is on CRAN so you can simply run :

``` r
install.packages("resourcecode")
```

You can install the development version of `{resourcecode}` thanks to
[r-universe](https://resourcecode-project.r-universe.dev/resourcecode):

``` r
install.packages('resourcecode', 
                 repos = c('https://resourcecode-project.r-universe.dev',
                           'https://cloud.r-project.org'))
```

or using the classical:

``` r
devtools::install_github("Resourcecode-project/r-resourcecode")
```

## Examples

Plot the bathymetry used in the project

``` r
library(resourcecode)
library(resourcecodedata)
#> Warning: le package 'resourcecodedata' a été compilé avec la version R 4.4.2
resourcecode::rscd_mapplot(rscd_field$depth, name = "Depth (m)", transform = "sqrt")
```

<img src="man/figures/README-plot-bathymetry-1.png" width="100%" style="display: block; margin: auto;" />

See the variables available in the database:

``` r
head(rscd_variables)
#>        name         longname         unit
#> 1 longitude        longitude  degree_east
#> 2  latitude         latitude degree_north
#> 3       tri              tri             
#> 4    MAPSTA       status map            1
#> 5       dpt            depth            m
#> 6      ucur eastward current        m s-1
```

Download a time series of significant wave height next to the coast of
Finistère:

``` r
data <- get_parameters(node = "134865", parameters = "hs")
str(data)
#> tibble [8,760 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ time: POSIXct[1:8760], format: "1994-01-01 01:00:00" "1994-01-01 02:00:00" ...
#>  $ hs  : num [1:8760] 4.98 5.19 5.37 5.48 5.52 ...
#>  - attr(*, "node")= num 134864
plot(data, type = "l")
```

<img src="man/figures/README-data_fetcher-1.png" width="100%" style="display: block; margin: auto;" />
