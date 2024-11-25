
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Resource**code** <a href="https://github.com/Resourcecode-project/r-resourcecode"><img src="man/figures/logo.png" align="right" height="139" alt="resourcecode website" /></a>

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
devtools::install_github("Resourcecode-project/r-resourcecode")
```

## Examples

Plot the bathymetry used in the project

``` r
library(resourcecode)
resourcecode::rscd_mapplot(resourcecode::rscd_field$depth, name = "Depth (m)", transform = "sqrt")
```

<img src="man/figures/README-plot-bathymetry-1.png" width="100%" style="display: block; margin: auto;" />

See the variables available in the database:

``` r
rscd_variables
#>         name
#> 1  longitude
#> 2   latitude
#> 3        tri
#> 4     MAPSTA
#> 5        dpt
#> 6       ucur
#> 7       vcur
#> 8       uwnd
#> 9       vwnd
#> 10       wlv
#> 11       d50
#> 12        hs
#> 13        lm
#> 14       t02
#> 15      t0m1
#> 16       t01
#> 17        fp
#> 18        tp
#> 19       dir
#> 20       spr
#> 21        dp
#> 22      phs0
#> 23      phs1
#> 24      phs2
#> 25      phs3
#> 26      phs4
#> 27      phs5
#> 28      ptp0
#> 29      ptp1
#> 30      ptp2
#> 31      ptp3
#> 32      ptp4
#> 33      ptp5
#> 34      plp0
#> 35      plp1
#> 36      plp2
#> 37      plp3
#> 38      plp4
#> 39      plp5
#> 40     pdir0
#> 41     pdir1
#> 42     pdir2
#> 43     pdir3
#> 44     pdir4
#> 45     pdir5
#> 46     pspr0
#> 47     pspr1
#> 48     pspr2
#> 49     pspr3
#> 50     pspr4
#> 51     pspr5
#> 52      pws0
#> 53      pws1
#> 54      pws2
#> 55      pws3
#> 56      pws4
#> 57      pws5
#> 58      pdp0
#> 59      pdp1
#> 60      pdp2
#> 61      pdp3
#> 62      pdp4
#> 63      pdp5
#> 64       tws
#> 65      uust
#> 66      vust
#> 67       cha
#> 68       cge
#> 69       faw
#> 70      utaw
#> 71      vtaw
#> 72      utwa
#> 73      vtwa
#> 74       wcc
#> 75      utwo
#> 76      vtwo
#> 77       foc
#> 78      utus
#> 79      vtus
#> 80      uuss
#> 81      vuss
#> 82      uabr
#> 83      vabr
#> 84      uubr
#> 85      vubr
#> 86      mssu
#> 87      mssc
#> 88      mssd
#>                                                                 longname
#> 1                                                              longitude
#> 2                                                               latitude
#> 3                                                                    tri
#> 4                                                             status map
#> 5                                                                  depth
#> 6                                                       eastward current
#> 7                                                      northward current
#> 8                                                          eastward_wind
#> 9                                                         northward_wind
#> 10                                    sea surface height above sea level
#> 11                                                            grain_size
#> 12                            significant height of wind and swell waves
#> 13                                                      mean wave length
#> 14                                                       mean period T02
#> 15                                                      mean period T0m1
#> 16                                                       mean period T01
#> 17                                                   wave peak frequency
#> 18                                        inverse of wave peak frequency
#> 19                                                   wave mean direction
#> 20                                                    directional spread
#> 21                                                        peak direction
#> 22                                   wave significant height partition 0
#> 23                                   wave significant height partition 1
#> 24                                   wave significant height partition 2
#> 25                                   wave significant height partition 3
#> 26                                   wave significant height partition 4
#> 27                                   wave significant height partition 5
#> 28                                               peak period partition 0
#> 29                                               peak period partition 1
#> 30                                               peak period partition 2
#> 31                                               peak period partition 3
#> 32                                               peak period partition 4
#> 33                                               peak period partition 5
#> 34                                          peak wave length partition 0
#> 35                                          peak wave length partition 1
#> 36                                          peak wave length partition 2
#> 37                                          peak wave length partition 3
#> 38                                          peak wave length partition 4
#> 39                                          peak wave length partition 5
#> 40                                       wave mean direction partition 0
#> 41                                       wave mean direction partition 1
#> 42                                       wave mean direction partition 2
#> 43                                       wave mean direction partition 3
#> 44                                       wave mean direction partition 4
#> 45                                       wave mean direction partition 5
#> 46                                        directional spread partition 0
#> 47                                        directional spread partition 1
#> 48                                        directional spread partition 2
#> 49                                        directional spread partition 3
#> 50                                        directional spread partition 4
#> 51                                        directional spread partition 5
#> 52                                      wind sea fraction in partition 0
#> 53                                      wind sea fraction in partition 1
#> 54                                      wind sea fraction in partition 2
#> 55                                      wind sea fraction in partition 3
#> 56                                      wind sea fraction in partition 4
#> 57                                      wind sea fraction in partition 5
#> 58                                            peak direction partition 0
#> 59                                            peak direction partition 1
#> 60                                            peak direction partition 2
#> 61                                            peak direction partition 3
#> 62                                            peak direction partition 4
#> 63                                            peak direction partition 5
#> 64                                                     wind sea fraction
#> 65                                            eastward friction velocity
#> 66                                           northward friction velocity
#> 67 charnock coefficient for surface roughness length for momentum in air
#> 68                                                      wave energy flux
#> 69                                              wind to wave energy flux
#> 70                                   eastward wave supported wind stress
#> 71                                  northward wave supported wind stress
#> 72                                          eastward wave to wind stress
#> 73                                         northward wave to wind stress
#> 74                                                     whitecap coverage
#> 75                                         eastward wave to ocean stress
#> 76                                        northward wave to ocean stress
#> 77                                             wave to ocean energy flux
#> 78                                             eastward stokes transport
#> 79                                            northward stokes transport
#> 80                                         eastward surface stokes drift
#> 81                                        northward surface stokes drift
#> 82                            rms of bottom displacement amplitude zonal
#> 83                       rms of bottom displacement amplitude meridional
#> 84                                rms of bottom velocity amplitude zonal
#> 85                           rms of bottom velocity amplitude meridional
#> 86                                            downwave mean square slope
#> 87                                           crosswave mean square slope
#> 88                                                   u direction for mss
#>                  unit
#> 1         degree_east
#> 2        degree_north
#> 3                    
#> 4                   1
#> 5                   m
#> 6               m s-1
#> 7               m s-1
#> 8               m s-1
#> 9               m s-1
#> 10                  m
#> 11 Krumbein phi scale
#> 12                  m
#> 13                  m
#> 14                  s
#> 15                  s
#> 16                  s
#> 17                s-1
#> 18              1/s-1
#> 19             degree
#> 20             degree
#> 21             degree
#> 22                  m
#> 23                  m
#> 24                  m
#> 25                  m
#> 26                  m
#> 27                  m
#> 28                  s
#> 29                  s
#> 30                  s
#> 31                  s
#> 32                  s
#> 33                  s
#> 34                  m
#> 35                  m
#> 36                  m
#> 37                  m
#> 38                  m
#> 39                  m
#> 40             degree
#> 41             degree
#> 42             degree
#> 43             degree
#> 44             degree
#> 45             degree
#> 46             degree
#> 47             degree
#> 48             degree
#> 49             degree
#> 50             degree
#> 51             degree
#> 52                  1
#> 53                  1
#> 54                  1
#> 55                  1
#> 56                  1
#> 57                  1
#> 58             degree
#> 59             degree
#> 60             degree
#> 61             degree
#> 62             degree
#> 63             degree
#> 64                  1
#> 65              m s-1
#> 66              m s-1
#> 67                  1
#> 68             kW m-1
#> 69              W m-2
#> 70             m2 s-2
#> 71             m2 s-2
#> 72             m2 s-2
#> 73             m2 s-2
#> 74                  1
#> 75             m2 s-2
#> 76             m2 s-2
#> 77              W m-2
#> 78             m2 s-1
#> 79             m2 s-1
#> 80              m s-1
#> 81              m s-1
#> 82                  m
#> 83                  m
#> 84              m s-1
#> 85              m s-1
#> 86                  1
#> 87                  1
#> 88             degree
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
```

``` r
plot(data, type = "l")
```

<img src="man/figures/README-data_fetcher-1.png" width="100%" style="display: block; margin: auto;" />
