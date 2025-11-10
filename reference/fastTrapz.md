# Fast implementation of pracma::trapz from the Armadillo C++ library

Compute the area of a multivariate function with (matrix) values Y at
the points x.

## Usage

``` r
fastTrapz(x, Y, dim = 1L)
```

## Arguments

- x:

  x-coordinates of points on the x-axis (vector)

- Y:

  y-coordinates of function values (matrix)

- dim:

  an integer giving the subscripts which the function will be applied
  over. 1 indicates rows, 2 indicates columns

## Value

a vector with one dimension less than Y

## Examples

``` r
x = 1:10
Y = sin(pi/10*matrix(1:10,ncol=10,nrow=10))
fastTrapz(x*pi/10,Y,2)
#>               [,1]
#>  [1,] 8.737250e-01
#>  [2,] 1.661924e+00
#>  [3,] 2.287442e+00
#>  [4,] 2.689049e+00
#>  [5,] 2.827433e+00
#>  [6,] 2.689049e+00
#>  [7,] 2.287442e+00
#>  [8,] 1.661924e+00
#>  [9,] 8.737250e-01
#> [10,] 3.462607e-16
```
