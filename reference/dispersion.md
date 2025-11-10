# Compute the dispersion relation of waves Find *k* s.t. (2.pi.f)^2 = g.k.tanh(k.d)

Compute the dispersion relation of waves Find *k* s.t. (2.pi.f)^2 =
g.k.tanh(k.d)

## Usage

``` r
dispersion(frequencies, depth, iter_max = 200, tol = 1e-06)
```

## Arguments

- frequencies:

  frequency vector

- depth:

  depth (m)

- iter_max:

  maximum number of iterations in the solver

- tol:

  tolerance for termination.

## Value

the wave numbers (same size as frequencies)

## Examples

``` r
freq <- seq(from = 0, to = 1, length.out = 100)
k1 <- dispersion(freq, depth = 1)
k10 <- dispersion(freq, depth = 10)
kInf <- dispersion(freq, depth = Inf)
plot(freq, k1, type = "l")
lines(freq, k10, col = "red")
lines(freq, kInf, col = "green")
```
