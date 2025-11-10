# Plot a wave density 1D spectrum at a given time

Plot a wave density 1D spectrum at a given time

## Usage

``` r
plot_1d_specta(spec, time = 1L, print_sea_state = TRUE, ...)
```

## Arguments

- spec:

  the spectral data, as an output from `get_2Dspectrum`

- time:

  the time to plot. Either an integer or the date.

- print_sea_state:

  should the sea_states parameters being plot ? Default to TRUE.

- ...:

  currently unused

## Value

a ggplot object

## Examples

``` r
plot_1d_specta(resourcecodedata::rscd_1d_spectra, 1)
```
