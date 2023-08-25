
# resourcecode

<!-- badges: start -->
<!-- badges: end -->

The goal of resourcecode is to provide an easy access to the Resourcecode hindcast database of sea-states. More information [here](https://resourcecode.ifremer.fr).

## Installation

The package is not yet on CRAN. Meanwhile, you can install the development version of `{resourcecode}` like so:

```{r}
devtools::install_github('NRaillard/resourcecode')
```

## Example

Plot the bathymetry used in the projet

```{r}
library(resourcecode)
resourcecode::rscd_mapplot(resourcecode::rscd_field$depth)
```

