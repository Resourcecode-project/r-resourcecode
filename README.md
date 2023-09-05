
# resourcecode <a href="https://nraillard.github.io/resourcecode/"><img src="man/figures/logo.png" align="right" height="139" alt="resourcecode website" /></a>

<!-- badges: start -->
<!-- badges: end -->

The goal of resourcecode is to provide an easy access to the Resourcecode hindcast database of sea-states. More information [here](https://resourcecode.ifremer.fr).

## Installation

The package is not yet on CRAN. Meanwhile, you can install the development version of `{resourcecode}` like so:

``` r
devtools::install_github('NRaillard/resourcecode')
```

## Examples

Plot the bathymetry used in the project

``` r
library(resourcecode)
resourcecode::rscd_mapplot(resourcecode::rscd_field$depth)
```
See the variables available in the database

``` r
rscd_variables
```


