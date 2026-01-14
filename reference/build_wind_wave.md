# Generate and Propagate a Wind Wave Over Variable Bathymetry

This function builds a wind-wave over input bathymetry and distances
returning an estimate of final wave height and related wave
characteristics (e.g., wave energy index, wave period, wave number,
celerity)

## Usage

``` r
build_wind_wave(fetch, depths, distances, wind_speed)
```

## Arguments

- fetch:

  Numeric. Total wind fetch distance (in meters).

- depths:

  Numeric vector Water depths (in meters) at each segment; must be one
  element longer than `distances`.

- distances:

  Numeric vector. Horizontal distances (in meters) between successive
  depth points. Must be one element shorter than `depths`.

- wind_speed:

  Numeric. Wind speed blowing from direction (in m/s).

## Value

A data frame with the following columns:

- wave_height_final:

  Final wave height at the end of the fetch (in meters).

- WEI:

  Wave Energy Index

- wave_period:

  Estimated wave period (in seconds).

- wave_number:

  Average wave number (radians per meter).

- celerity_final:

  Final wave celerity (phase speed, in m/s).

- nnumber_final:

  Final group velocity coefficient.

## Details

Wave growth is calculated segment-by-segment, adjusting for shoaling,
wave breaking, and changing depth. Deep and shallow water regimes are
handled using different formulas. The function uses a Newton-Raphson
method to compute wave number.

## Examples

``` r
fetch <- 90
depths <- c(1, 2, 3, 5, 5)
distances <- c(25, 25, 25, 15)
wind_speed <- 10
wind_proportion <- 0.25
result <- build_wind_wave(fetch, depths, distances, wind_speed)
```
