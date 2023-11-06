
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Resource**code** <a href="https://nraillard.github.io/resourcecode/"><img src="man/figures/logo.png" align="right" height="139" alt="resourcecode website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Resourcecode-project/r-resourcecode/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/Resourcecode-project/r-resourcecode/actions)
[![Codecov test
coverage](https://codecov.io/gh/NRaillard/resourcecode/branch/main/graph/badge.svg)](https://app.codecov.io/gh/NRaillard/resourcecode?branch=main)
<!-- badges: end -->

The goal of `{resourcecode}` is to provide an easy access to the
Resourcecode hindcast database of sea-states. More information on the
database can be found [here](https://resourcecode.ifremer.fr). This
website only contains information about the way to retrieve data from
this data base and the functionalities offred by this package.

## Installation

The package is not yet on CRAN. Meanwhile, you can install the
development version of `{resourcecode}` like so:

``` r
devtools::install_github('Resourcecode-project/r-resourcecode')
```

## Examples

Plot the bathymetry used in the project

``` r
library(resourcecode)
resourcecode::rscd_mapplot(resourcecode::rscd_field$depth)
```

<img src="man/figures/README-plot-bathymetry-1.png" width="100%" style="display: block; margin: auto;" />

See the variables available in the database:

``` r
rscd_variables
#> # A tibble: 88 × 3
#>    name      longname                           unit          
#>    <chr>     <chr>                              <chr>         
#>  1 longitude longitude                          "degree_east" 
#>  2 latitude  latitude                           "degree_north"
#>  3 tri       tri                                ""            
#>  4 MAPSTA    status map                         "1"           
#>  5 dpt       depth                              "m"           
#>  6 ucur      eastward current                   "m s-1"       
#>  7 vcur      northward current                  "m s-1"       
#>  8 uwnd      eastward_wind                      "m s-1"       
#>  9 vwnd      northward_wind                     "m s-1"       
#> 10 wlv       sea surface height above sea level "m"           
#> # ℹ 78 more rows
```

Download a time series of significant wave height next to the coast of
Finistère:

``` r
data = get_parameters(node="134865",parameters = "hs")
str(data)
#> tibble [8,760 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ time: POSIXct[1:8760], format: "1994-01-01 01:00:00" "1994-01-01 02:00:00" ...
#>  $ hs  : num [1:8760] 4.98 5.19 5.37 5.48 5.52 ...
#>  - attr(*, "node")= num 134864
plot(data,type='l')
```

<img src="man/figures/README-data_fetcher-1.png" width="100%" style="display: block; margin: auto;" />
