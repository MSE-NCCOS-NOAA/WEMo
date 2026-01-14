# Run the WEMo Wave Energy Model

`wemo()` Combines fetch, bathymetry, and wind data to estimate wave
height and energy metrics at each site using WEMo (Wave Energy Model)
from wind-waves.

First, the function builds a wind-wave over input bathymetry and returns
an estimate of final wave height and related wave characteristics (e.g.,
wave energy index, wave period, wave number, celerity).

Then, the function summarizes the waves from every direction for each
site and computes the summary statistic `RWE`.

## Usage

``` r
wemo(fetch)
```

## Arguments

- fetch:

  A data frame or `sf` object containing fetch geometry and required
  variables: `efetch`, `depths`, `distances`, `speed`, `proportion`.
  Expects the output from
  [`prepare_wemo_inputs()`](https://mse-nccos-noaa.github.io/WEMo/reference/prepare_wemo_inputs.md).
  Each row corresponds to a fetch ray.

## Value

A list of two elements:

- `wemo_details`:

  the original `fetch` object with the following stats about each wave
  arriving at the site from the direction of the fetch ray added:

  - `wave_height_final` - Final wave height at the end of the fetch (m)

  - `WEI` - Wave Energy Index

  - `wave_period` - Estimated wave period (s)

  - `wave_number` - Average wave number (rad/m)

  - `celerity_final` - Final wave phase speed (m/s)

  - `nnumber_final` - Final group velocity coefficient

- `wemo_final`:

  sf object with all the variables from `site_point` in addition to the
  following:

  - `RWE` - Total relative wave exposure at the site

  - `avg_wave_height` - Mean wave height across all fetch rays (m)

  - `max_wave_height` - Maximum wave height across all fetch rays (m)

  - `direction_of_max_wave` - Direction(s) of maximum wave height
    (Degrees from North)

## Details

This function builds and propagates waves arriving at the site from each
fetch ray segment-by-segment, adjusting for shoaling, wave breaking, and
changing depth. Deep and shallow water regimes are handled using
different formulas. The function uses a Newton-Raphson method to compute
wave number. The values in the `wemo_final` return are calculated from
summarizing (mean and max wave height) of all fetch rays. Relative Wave
Energy (`RWE`) is calculated by multiplying the `REI` from each wave by
the proportion of time that wind blows from the direction of the ray
each ray and summing for all fetch rays

## See also

[`prepare_wemo_inputs()`](https://mse-nccos-noaa.github.io/WEMo/reference/prepare_wemo_inputs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example assumes fetch object includes efetch, depths, distances, speed, and direction
result <- wemo(fetch = fetch, site_points = site_pts)
details <- result$wemo_details
summary <- result$wemo_output
} # }
```
